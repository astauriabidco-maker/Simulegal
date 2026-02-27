import { Injectable, Logger, BadRequestException, ForbiddenException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { ConfigService } from '@nestjs/config';
import OpenAI from 'openai';

@Injectable()
export class AiReportingService {
    private readonly logger = new Logger(AiReportingService.name);
    private openai: OpenAI | null = null;
    private readonly isAiEnabled: boolean;

    // A summarized schema to feed to the LLM
    private readonly SCHEMA_SUMMARY = `
SQLite Database Schema for SimuLegal CRM:
- Lead(id, name, email, serviceId, serviceName, amountPaid, status, originAgencyId, createdAt)
  Statuses: NEW, REVIEW, PAID, processing, DONE, SECURE, etc.
- Agency(id, name, type, status, region, city)
- Transaction(id, type, amount, leadId, createdAt)
- Appointment(id, type, status, leadId, agencyId, createdAt)
- User(id, email, name, role, agencyId, createdAt)

Guidelines:
- Return ONLY valid JSON, no markdown formatting.
- The JSON must match the following structure precisely:
{
  "sql": "A safe SELECT query in SQLite to get the data",
  "widget": {
    "type": "BAR_CHART" | "PIE_CHART" | "KPI_CARD" | "TABLE",
    "title": "A clear title for the widget",
    "description": "A short insight or description of the data"
  },
  "mapping": {
    "labelColumn": "The name of the column that represents the labels (e.g., month, agency_name, status)",
    "valueColumn": "The name of the column that represents the numeric values (e.g., total_revenue, count)"
  }
}
- DO NOT use any mutating operations (INSERT, UPDATE, DELETE).
- Group by dates using SQLite functions like strftime('%Y-%m', createdAt).
`;

    constructor(
        private prisma: PrismaService,
        private configService: ConfigService,
    ) {
        const apiKey = this.configService.get<string>('OPENAI_API_KEY');
        if (apiKey) {
            this.openai = new OpenAI({ apiKey });
            this.isAiEnabled = true;
            this.logger.log('AI Reporting Service initialized with OpenAI.');
        } else {
            this.isAiEnabled = false;
            this.logger.warn('OPENAI_API_KEY is not set. AI Reporting will work in mock mode.');
        }
    }

    async generateWidget(prompt: string, userRole: string, userAgencyId?: string) {
        this.logger.log(`Generating widget for prompt: "${prompt}" [Role: ${userRole}]`);

        // Check if user has permission
        if (userRole !== 'SUPER_ADMIN' && userRole !== 'HQ_ADMIN' && userRole !== 'SUPERADMIN' && userRole !== 'HQ') {
            // Let's restrict data to their agency if they are agency manager
            if (userRole === 'AGENCY_MANAGER' || userRole === 'AGENCY') {
                prompt += `\nConstraint: ONLY consider data for agencyId = '${userAgencyId}'.`;
            } else {
                throw new ForbiddenException('Not authorized to access AI Reporting.');
            }
        }

        let aiResponseJson;

        if (this.isAiEnabled && this.openai) {
            try {
                const response = await this.openai.chat.completions.create({
                    model: 'gpt-4o',
                    messages: [
                        { role: 'system', content: this.SCHEMA_SUMMARY },
                        { role: 'user', content: prompt }
                    ],
                    temperature: 0,
                    response_format: { type: "json_object" }
                });

                const content = response.choices[0].message.content;
                if (!content) throw new Error('Empty response from OpenAI');

                aiResponseJson = JSON.parse(content);
            } catch (error) {
                this.logger.error('OpenAI Error', error);
                throw new BadRequestException('Failed to generate query from prompt.');
            }
        } else {
            // Mock Mode for development
            aiResponseJson = this.getMockResponse(prompt);
        }

        const { sql, widget, mapping } = aiResponseJson;

        // Security check
        if (!sql.trim().toUpperCase().startsWith('SELECT')) {
            throw new BadRequestException('Invalid query generated. Only SELECT operations are allowed.');
        }

        try {
            // Execute the query
            this.logger.debug(`Executing generated SQL: ${sql}`);
            const rawData = await this.prisma.$queryRawUnsafe<any[]>(sql);

            // Format data for the widget
            let formattedData = [];
            if (widget.type === 'KPI_CARD') {
                formattedData = rawData[0] ? rawData[0][mapping.valueColumn] || 0 : 0;
            } else if (widget.type === 'TABLE') {
                formattedData = rawData;
            } else {
                formattedData = rawData.map(row => ({
                    label: row[mapping.labelColumn]?.toString() || 'Inconnu',
                    value: Number(row[mapping.valueColumn]) || 0
                }));
            }

            return {
                widget: {
                    ...widget,
                    data: formattedData,
                    config: {
                        sql,
                        mapping,
                        prompt
                    }
                },
                debug: { sql } // Expose SQL for debugging in UI
            };

        } catch (error) {
            this.logger.error(`Database execution error for SQL: ${sql}`, error);
            throw new BadRequestException('The generated query failed to execute against the database.');
        }
    }

    private getMockResponse(prompt: string) {
        // Fallback mock responses if no API key
        if (prompt.toLowerCase().includes('chiffre d\'affaire') || prompt.toLowerCase().includes('ca')) {
            return {
                sql: "SELECT strftime('%Y-%m', createdAt) as month, SUM(amountPaid) as total_revenue FROM Lead WHERE status = 'PAID' OR status = 'COMPLETED' GROUP BY month ORDER BY month DESC LIMIT 6;",
                widget: { type: "BAR_CHART", title: "Évolution du Chiffre d'Affaires", description: "Basé sur les leads convertis." },
                mapping: { labelColumn: "month", valueColumn: "total_revenue" }
            };
        }

        return {
            sql: "SELECT status, COUNT(*) as count FROM Lead GROUP BY status;",
            widget: { type: "PIE_CHART", title: "Répartition des Leads par Statut", description: "Vue globale du tunnel." },
            mapping: { labelColumn: "status", valueColumn: "count" }
        };
    }

    async saveWidget(userId: string, agencyId: string, widgetData: any) {
        if (!widgetData.config) {
            throw new BadRequestException("Widget configuration missing");
        }
        return this.prisma.aiDashboardWidget.create({
            data: {
                userId,
                agencyId,
                title: widgetData.title,
                description: widgetData.description || '',
                widgetType: widgetData.type,
                prompt: widgetData.config.prompt,
                querySql: widgetData.config.sql,
                mappingConfig: JSON.stringify(widgetData.config.mapping),
            }
        });
    }

    async getUserWidgets(userId: string) {
        const widgets = await this.prisma.aiDashboardWidget.findMany({
            where: { userId },
            orderBy: { position: 'asc' }
        });

        // Pou chaque widget, executer dynamiquement la requête SQL pour avoir une donnée Live !
        const liveWidgets = await Promise.all(widgets.map(async (w) => {
            try {
                const rawData = await this.prisma.$queryRawUnsafe<any[]>(w.querySql);
                const mapping = JSON.parse(w.mappingConfig);
                let formattedData: any = [];
                if (w.widgetType === 'KPI_CARD') {
                    formattedData = rawData[0] ? rawData[0][mapping.valueColumn] || 0 : 0;
                } else if (w.widgetType === 'TABLE') {
                    formattedData = rawData;
                } else {
                    formattedData = rawData.map(row => ({
                        label: row[mapping.labelColumn]?.toString() || 'Inconnu',
                        value: Number(row[mapping.valueColumn]) || 0
                    }));
                }
                return {
                    id: w.id,
                    type: w.widgetType,
                    title: w.title,
                    description: w.description,
                    data: formattedData,
                };
            } catch (err) {
                return {
                    id: w.id,
                    type: w.widgetType,
                    title: w.title,
                    description: "Erreur lors du chargement des données",
                    data: [],
                    error: true
                };
            }
        }));

        return liveWidgets;
    }

    async deleteWidget(id: string, userId: string) {
        return this.prisma.aiDashboardWidget.deleteMany({
            where: { id, userId }
        });
    }
}
