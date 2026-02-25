import { Injectable, BadRequestException, Inject, forwardRef } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Prospect, ProspectStatus } from '@prisma/client';
import { AssignmentService } from './assignment.service';
import { ProspectPipelineService } from './prospect-pipeline.service';
import { AppointmentsService } from '../appointments/appointments.service';
import { SERVICE_CATALOG, DOCUMENT_CATALOG } from '../config/services-pipeline.config';

@Injectable()
export class SalesService {
    constructor(
        private prisma: PrismaService,
        private assignmentService: AssignmentService,
        private prospectPipeline: ProspectPipelineService,
        @Inject(forwardRef(() => AppointmentsService))
        private appointmentsService: AppointmentsService,
    ) { }

    async findAll(params: {
        page: number;
        limit: number;
        status?: string;
        agencyId?: string;
        source?: string;
        dateFrom?: string;
        dateTo?: string;
    }) {
        const { page, limit, status, agencyId, source, dateFrom, dateTo } = params;
        const skip = (page - 1) * limit;

        const where: any = {};

        // Status filter
        if (status) {
            where.status = status;
        }

        // Agency filter
        if (agencyId) {
            where.agencyId = agencyId;
        }

        // Source filter
        if (source) {
            where.source = source;
        }

        // Date range filter
        if (dateFrom || dateTo) {
            where.createdAt = {};
            if (dateFrom) {
                where.createdAt.gte = new Date(dateFrom);
            }
            if (dateTo) {
                where.createdAt.lte = new Date(dateTo);
            }
        }

        const [data, total] = await Promise.all([
            this.prisma.prospect.findMany({
                where,
                skip,
                take: limit,
                include: { notes: true },
                orderBy: { createdAt: 'desc' },
            }),
            this.prisma.prospect.count({ where })
        ]);

        return {
            data,
            meta: {
                total,
                page,
                limit,
                totalPages: Math.ceil(total / limit)
            }
        };
    }

    async exportToCSV(filters?: { agencyId?: string; source?: string; dateFrom?: string; dateTo?: string }): Promise<string> {
        const where: any = {};

        if (filters?.agencyId) where.agencyId = filters.agencyId;
        if (filters?.source) where.source = filters.source;
        if (filters?.dateFrom || filters?.dateTo) {
            where.createdAt = {};
            if (filters.dateFrom) where.createdAt.gte = new Date(filters.dateFrom);
            if (filters.dateTo) where.createdAt.lte = new Date(filters.dateTo);
        }

        const prospects = await this.prisma.prospect.findMany({
            where,
            orderBy: { createdAt: 'desc' },
        });

        // Build CSV
        const headers = ['ID', 'Prénom', 'Nom', 'Téléphone', 'Email', 'Source', 'Agence', 'Statut', 'Score', 'Date création'];
        const rows = prospects.map(p => [
            p.id,
            p.firstName,
            p.lastName,
            p.phone,
            p.email || '',
            p.source,
            p.agencyId,
            p.status,
            p.score.toString(),
            new Date(p.createdAt).toLocaleDateString('fr-FR')
        ].map(v => `"${(v || '').replace(/"/g, '""')}"`).join(';'));

        return [headers.join(';'), ...rows].join('\n');
    }

    async findOne(id: string) {
        return this.prisma.prospect.findUnique({
            where: { id },
            include: { notes: true },
        });
    }

    async create(data: any) {
        const score = this.calculateScore(data);

        // Round-robin assignment to a sales agent
        const assignedToSalesId = await this.assignmentService.getNextSalesAgent(data.agencyId || null);

        const prospect = await this.prisma.prospect.create({
            data: {
                ...data,
                score,
                status: 'NEW',
                assignedToSalesId,
            },
        });

        await this.triggerAutomation(prospect, 'NEW');
        return prospect;
    }

    async update(id: string, data: any) {
        const oldProspect = await this.prisma.prospect.findUnique({ where: { id } });
        if (!oldProspect) return null;

        // ─── Sanitize: only pass valid Prospect scalar fields to Prisma ───
        const ALLOWED_FIELDS = [
            'firstName', 'lastName', 'phone', 'email',
            'address', 'city', 'zipCode', 'country',
            'source', 'campaignName', 'interestServiceId', 'score',
            'agencyId', 'assignedToSalesId', 'status', 'convertedLeadId',
            'lastContactAt',
            'callAttempts', 'noAnswerCount', 'callbackCount',
            'callbackRequestedAt', 'callbackScheduledAt', 'lastCallOutcome',
            'noShowCount', 'qualifiedAt', 'stageEnteredAt', 'lostReason',
            'appointment', // Json field
            'eligibilityResult', // Json field — résultat de simulation
        ];

        const sanitizedData: any = {};
        for (const key of ALLOWED_FIELDS) {
            if (data[key] !== undefined) {
                sanitizedData[key] = data[key];
            }
        }

        // Convert date strings to Date objects for DateTime fields
        const DATE_FIELDS = ['lastContactAt', 'callbackRequestedAt', 'callbackScheduledAt', 'qualifiedAt', 'stageEnteredAt'];
        for (const field of DATE_FIELDS) {
            if (sanitizedData[field] && typeof sanitizedData[field] === 'string') {
                sanitizedData[field] = new Date(sanitizedData[field]);
            }
        }

        // ─── Auto-track stage entry time on status change ───
        if (sanitizedData.status && sanitizedData.status !== oldProspect.status) {
            sanitizedData.stageEnteredAt = new Date();

            // Track qualification date
            if (sanitizedData.status === 'QUALIFIED') {
                sanitizedData.qualifiedAt = new Date();
            }

            // Track NO_SHOW count and auto-LOST
            if (sanitizedData.status === 'NO_SHOW') {
                const newNoShowCount = (oldProspect.noShowCount || 0) + 1;
                sanitizedData.noShowCount = newNoShowCount;
                if (newNoShowCount >= 2) {
                    sanitizedData.status = 'LOST';
                    sanitizedData.lostReason = `${newNoShowCount} RDV non honorés`;
                    console.log(`[PIPELINE] ⚠️ ${oldProspect.firstName} auto-LOST: ${newNoShowCount} no-shows`);
                }
            }

            // Track lost reason
            if (sanitizedData.status === 'LOST' && !sanitizedData.lostReason) {
                sanitizedData.lostReason = data.lostReason || 'Manuel';
            }
        }

        // ─── Dynamic score recalculation on interaction ───
        if (sanitizedData.lastCallOutcome || sanitizedData.status) {
            const newScore = this.recalculateScore(oldProspect, sanitizedData);
            sanitizedData.score = newScore;
        }

        const updatedProspect = await this.prisma.prospect.update({
            where: { id },
            data: sanitizedData,
            include: { notes: true },
        });

        // Trigger notification automations on status change
        if (sanitizedData.status && sanitizedData.status !== oldProspect.status) {
            await this.triggerAutomation(updatedProspect, sanitizedData.status);
        }

        // ─── RÈGLE 2 : Auto-qualification check ───────────────
        if (sanitizedData.score !== undefined || sanitizedData.zipCode !== undefined || sanitizedData.interestServiceId !== undefined) {
            await this.prospectPipeline.checkQualification(id);
        }

        // ─── RÈGLE 3 & 5 : Appointment booked ─────────────────
        if (sanitizedData.status === 'MEETING_BOOKED' && oldProspect.status !== 'MEETING_BOOKED') {
            await this.prospectPipeline.onAppointmentBooked(id, new Date().toISOString());
        }

        return updatedProspect;
    }

    /**
     * Fixer un rendez-vous en agence pour un prospect
     * ─── TEMPS RÉEL ───
     * 1. Vérifie la disponibilité du créneau via AppointmentsService
     * 2. Auto-assigne un juriste/agent disponible (hostUser)
     * 3. Crée le SalesAppointment + Appointment calendrier
     * 4. Met à jour le prospect
     */
    async bookAppointment(prospectId: string, appointmentData: {
        date: string;
        agencyId: string;
        agencyName: string;
        serviceId?: string;
        confirmed?: boolean;
        confirmationSentVia?: string;
    }) {
        const prospect = await this.prisma.prospect.findUnique({ where: { id: prospectId } });
        if (!prospect) return null;

        const startDate = new Date(appointmentData.date);
        const endDate = new Date(startDate.getTime() + 60 * 60 * 1000); // +1h
        const serviceId = appointmentData.serviceId || prospect.interestServiceId || undefined;

        // ─── 0. Vérifier la disponibilité temps réel ───
        const availableSlots = await this.appointmentsService.getAvailableSlots(
            startDate.toISOString().split('T')[0],
            appointmentData.agencyId,
            serviceId,
        );

        // Vérifier que le créneau demandé est dans la liste des slots disponibles
        const requestedSlotIso = startDate.toISOString();
        const isSlotAvailable = availableSlots.some(slot => {
            const slotDate = new Date(slot);
            return Math.abs(slotDate.getTime() - startDate.getTime()) < 60000; // marge 1 min
        });

        if (!isSlotAvailable) {
            throw new BadRequestException(
                'Ce créneau n\'est plus disponible. Un autre rendez-vous a été pris entre-temps. Veuillez rafraîchir les créneaux.'
            );
        }

        // ─── 1. Auto-assigner un juriste/agent disponible ───
        const hostUserId = await this.appointmentsService.findAvailableHost(
            requestedSlotIso,
            appointmentData.agencyId,
            serviceId,
        );

        if (!hostUserId) {
            throw new BadRequestException(
                'Aucun juriste ou agent disponible sur ce créneau pour le service demandé. Veuillez choisir un autre créneau.'
            );
        }

        // ─── 2. Créer le SalesAppointment (suivi commercial) ───
        const salesAppointment = await this.prisma.salesAppointment.create({
            data: {
                prospectId,
                date: startDate,
                agencyId: appointmentData.agencyId,
                agencyName: appointmentData.agencyName,
                serviceId: serviceId,
                status: 'SCHEDULED',
                confirmationSent: appointmentData.confirmed || false,
                confirmationSentVia: appointmentData.confirmationSentVia,
            }
        });

        // ─── 3. Créer l'Appointment dans le calendrier global (avec hostUser) ───
        let calendarAppointment = null;
        try {
            const agencyExists = await this.prisma.agency.findUnique({
                where: { id: appointmentData.agencyId }
            });

            calendarAppointment = await this.prisma.appointment.create({
                data: {
                    start: startDate,
                    end: endDate,
                    type: 'PHYSICAL_AGENCY',
                    status: 'SCHEDULED',
                    leadName: `${prospect.firstName} ${prospect.lastName}`,
                    leadEmail: prospect.email,
                    prospectId: prospectId,
                    hostUserId: hostUserId,
                    ...(agencyExists ? { agencyId: appointmentData.agencyId } : {}),
                    serviceId: serviceId,
                }
            });
            console.log(`[SalesService] 📆 Appointment créé: ${calendarAppointment.id} — assigné à ${hostUserId}`);
        } catch (calError) {
            console.warn('[SalesService] ⚠️ Impossible de créer le RDV dans le calendrier:', calError);
        }

        // Récupérer le nom du host pour l'afficher dans le prospect
        let hostName = '';
        try {
            const host = await this.prisma.user.findUnique({ where: { id: hostUserId }, select: { name: true } });
            hostName = host?.name || '';
        } catch (e) { /* ignore */ }

        // ─── 4. Mettre à jour le prospect ───
        const updatedProspect = await this.prisma.prospect.update({
            where: { id: prospectId },
            data: {
                status: 'MEETING_BOOKED',
                appointment: {
                    ...appointmentData,
                    hostUserId,
                    hostName,
                    calendarAppointmentId: calendarAppointment?.id,
                } as any,
                lastContactAt: new Date(),
            }
        });

        // ─── 5. Trigger pipeline automation ───
        await this.prospectPipeline.onAppointmentBooked(prospectId, appointmentData.date);

        console.log(`[SalesService] 📅 RDV temps-réel: ${salesAppointment.id} — prospect ${prospectId} — ${appointmentData.date} @ ${appointmentData.agencyName} — juriste: ${hostName || hostUserId}`);

        return {
            prospect: updatedProspect,
            appointment: salesAppointment,
            calendarAppointmentId: calendarAppointment?.id,
            assignedHost: { id: hostUserId, name: hostName },
        };
    }

    /**
     * Récupérer tous les RDV (pour le calendrier)
     */
    async getAppointments(filters?: { agencyId?: string; status?: string; dateFrom?: string; dateTo?: string }) {
        const where: any = {};
        if (filters?.agencyId) where.agencyId = filters.agencyId;
        if (filters?.status) where.status = filters.status;
        if (filters?.dateFrom || filters?.dateTo) {
            where.date = {};
            if (filters.dateFrom) where.date.gte = new Date(filters.dateFrom);
            if (filters.dateTo) where.date.lte = new Date(filters.dateTo);
        }

        return this.prisma.salesAppointment.findMany({
            where,
            include: {
                prospect: {
                    select: {
                        id: true,
                        firstName: true,
                        lastName: true,
                        phone: true,
                        email: true,
                        interestServiceId: true,
                    }
                }
            },
            orderBy: { date: 'asc' },
        });
    }

    /**
     * Score dynamique — recalcule le score en fonction de l'état actuel et des interactions
     */
    private recalculateScore(prospect: any, updates: any): number {
        let score = 0;

        // ─── Base : Source d'acquisition ───
        const source = updates.source || prospect.source;
        if (source === 'GOOGLE_ADS') score += 30;
        else if (source === 'META_ADS') score += 20;
        else if (source === 'TIKTOK_ADS') score += 15;
        else if (source === 'REFERRAL') score += 25;
        else if (source === 'WEBSITE') score += 10;
        else score += 5;

        // ─── Complétude du profil ───
        const email = updates.email !== undefined ? updates.email : prospect.email;
        const phone = updates.phone !== undefined ? updates.phone : prospect.phone;
        const interestServiceId = updates.interestServiceId !== undefined ? updates.interestServiceId : prospect.interestServiceId;
        const address = updates.address !== undefined ? updates.address : prospect.address;
        const zipCode = updates.zipCode !== undefined ? updates.zipCode : prospect.zipCode;

        if (email && email.length > 5) score += 5;
        if (phone && phone.length > 8) score += 5;
        if (interestServiceId) score += 10;
        if (address) score += 5;
        if (zipCode) score += 5;

        // ─── Engagement (interactions positives) ───
        const outcome = updates.lastCallOutcome || prospect.lastCallOutcome;
        if (outcome === 'INTERESTED') score += 20;
        else if (outcome === 'CALLBACK') score += 10;
        else if (outcome === 'NO_ANSWER') score -= 5;
        else if (outcome === 'NOT_INTERESTED') score -= 30;
        else if (outcome === 'WRONG_NUMBER') score -= 40;

        // ─── Progression pipeline ───
        const status = updates.status || prospect.status;
        if (status === 'CONTACTED') score += 5;
        if (status === 'QUALIFIED') score += 15;
        if (status === 'MEETING_BOOKED') score += 25;
        if (status === 'NO_SHOW') score -= 10;
        if (status === 'LOST') score -= 20;

        // ─── Pénalités ───
        const noAnswerCount = updates.noAnswerCount !== undefined ? updates.noAnswerCount : (prospect.noAnswerCount || 0);
        score -= noAnswerCount * 3;

        return Math.max(0, Math.min(100, score));
    }

    private calculateScore(prospect: any): number {
        return this.recalculateScore(prospect, {});
    }

    private async triggerAutomation(prospect: Prospect, status: ProspectStatus) {
        console.log(`[BACKEND AUTOMATION] Trigger for ${prospect.firstName} -> ${status}`);

        switch (status) {
            case 'NEW':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, merci de votre intérêt pour Simulegal. Un expert va vous rappeler ds les 2h."`);
                break;
            case 'QUALIFIED':
                console.log(`[SMS] 📤 To ${prospect.phone}: "${prospect.firstName}, votre dossier a été pré-qualifié ! Un conseiller va vous proposer un RDV en agence."`);
                break;
            case 'MEETING_BOOKED':
                console.log(`[EMAIL] 📧 To ${prospect.email}: "Votre RDV Simulegal est confirmé."`);
                break;
            case 'NO_SHOW':
                console.log(`[SMS] 📤 To ${prospect.phone}: "Bonjour ${prospect.firstName}, nous avons remarqué que vous n'avez pas pu venir. Souhaitez-vous reprogrammer ?"`);
                break;
            case 'SIGNED':
                console.log(`[EMAIL] 📧 To ${prospect.email}: "Bienvenue chez Simulegal ! Votre dossier est ouvert."`);
                break;
            case 'LOST':
                console.log(`[PIPELINE] 📋 ${prospect.firstName} ${prospect.lastName} marqué comme perdu.`);
                break;
        }
    }

    async addNote(prospectId: string, authorId: string, text: string) {
        return this.prisma.prospectNote.create({
            data: {
                prospectId,
                authorId: authorId || 'system',
                text,
            },
        });
    }

    /**
     * Réactiver un prospect perdu — retour en CONTACTED
     */
    async reactivateProspect(id: string) {
        const prospect = await this.prisma.prospect.findUnique({ where: { id } });
        if (!prospect || prospect.status !== 'LOST') return null;

        const updated = await this.prisma.prospect.update({
            where: { id },
            data: {
                status: 'CONTACTED',
                stageEnteredAt: new Date(),
                noAnswerCount: 0,
                callbackCount: 0,
                lastCallOutcome: null,
                lostReason: null,
                score: this.recalculateScore({ ...prospect, status: 'CONTACTED', lastCallOutcome: null, noAnswerCount: 0 }, {}),
            },
            include: { notes: true },
        });

        // Add system note
        await this.addNote(id, 'system', '♻️ Prospect réactivé — retour en file de prospection');

        console.log(`[PIPELINE] ♻️ ${prospect.firstName} ${prospect.lastName} réactivé (était LOST: ${prospect.lostReason})`);
        return updated;
    }

    /**
     * Métriques de vélocité du pipeline
     */
    async getPipelineVelocity() {
        const prospects = await this.prisma.prospect.findMany({
            select: {
                id: true, status: true, score: true,
                createdAt: true, stageEnteredAt: true, qualifiedAt: true,
                callAttempts: true, noShowCount: true, callbackScheduledAt: true,
                lastCallOutcome: true, noAnswerCount: true, callbackCount: true,
                firstName: true, lastName: true,
            }
        });

        const now = new Date();
        const statusCounts: Record<string, number> = {};
        const avgTimeInStage: Record<string, number[]> = {};
        let staleLeads = 0;
        let overdueCallbacks = 0;

        for (const p of prospects) {
            // Count by status
            statusCounts[p.status] = (statusCounts[p.status] || 0) + 1;

            // Time in current stage (hours)
            if (p.stageEnteredAt) {
                const hoursInStage = (now.getTime() - new Date(p.stageEnteredAt).getTime()) / (1000 * 60 * 60);
                if (!avgTimeInStage[p.status]) avgTimeInStage[p.status] = [];
                avgTimeInStage[p.status].push(hoursInStage);

                // Stale: in NEW for > 48h without action
                if (p.status === 'NEW' && hoursInStage > 48 && p.callAttempts === 0) {
                    staleLeads++;
                }
            }

            // Overdue callbacks
            if (p.callbackScheduledAt && new Date(p.callbackScheduledAt) < now && p.lastCallOutcome === 'CALLBACK') {
                overdueCallbacks++;
            }
        }

        // Calculate averages
        const avgByStage: Record<string, number> = {};
        for (const [status, times] of Object.entries(avgTimeInStage)) {
            avgByStage[status] = Math.round(times.reduce((a, b) => a + b, 0) / times.length);
        }

        // Conversion rates
        const total = prospects.length || 1;
        const contacted = prospects.filter(p => !['NEW'].includes(p.status)).length;
        const qualified = prospects.filter(p => !['NEW', 'CONTACTED'].includes(p.status)).length;
        const meetingBooked = prospects.filter(p => ['MEETING_BOOKED', 'SIGNED'].includes(p.status)).length;
        const signed = prospects.filter(p => p.status === 'SIGNED').length;

        return {
            statusCounts,
            avgHoursInStage: avgByStage,
            conversionRates: {
                newToContacted: Math.round((contacted / total) * 100),
                contactedToQualified: contacted ? Math.round((qualified / contacted) * 100) : 0,
                qualifiedToMeeting: qualified ? Math.round((meetingBooked / qualified) * 100) : 0,
                meetingToSigned: meetingBooked ? Math.round((signed / meetingBooked) * 100) : 0,
                overall: Math.round((signed / total) * 100),
            },
            alerts: {
                staleLeads,
                overdueCallbacks,
            },
            totalProspects: total,
        };
    }

    async importFromCSV(buffer: Buffer): Promise<number> {
        const content = buffer.toString('utf-8');
        const lines = content.split(/\r?\n/);

        let count = 0;
        const headers = lines[0]?.toLowerCase().split(/[;,]/).map(h => h.trim().replace(/"/g, ''));

        // Basic validation
        if (!headers || !headers.includes('firstname') || !headers.includes('lastname')) {
            throw new Error('Invalid CSV headers. Expected firstName, lastName, phone, email');
        }

        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;

            const values = line.split(/[;,]/).map(v => v.trim().replace(/"/g, ''));
            const data: any = {};

            headers.forEach((header, index) => {
                const val = values[index];
                if (header === 'firstname') data.firstName = val;
                if (header === 'lastname') data.lastName = val;
                if (header === 'email') data.email = val;
                if (header === 'phone') data.phone = val;
                if (header === 'source') data.source = val;
            });

            if (data.firstName && data.lastName) {
                // Set defaults
                data.source = data.source || 'CSV_IMPORT';
                data.status = 'NEW';
                data.score = 10; // Base score for imported
                data.agencyId = data.agencyId || 'HQ-001'; // Default to HQ

                // Round-robin assignment
                data.assignedToSalesId = await this.assignmentService.getNextSalesAgent(data.agencyId);

                await this.prisma.prospect.create({ data });
                count++;
            }
        }

        return count;
    }

    // ═══════════════════════════════════════════════════════════
    // CONVERSION : PROSPECT → LEAD CRM
    // ═══════════════════════════════════════════════════════════

    /**
     * Convertit un prospect signé en Lead CRM complet.
     * Transfère toutes les données et crée le lien bidirectionnel.
     */
    async convertToLead(prospectId: string, serviceId?: string): Promise<{ leadId: string; prospect: any } | null> {
        const prospect = await this.prisma.prospect.findUnique({
            where: { id: prospectId },
            include: { notes: true, callLogs: true },
        });

        if (!prospect) return null;

        // Vérifier que le prospect peut être converti
        const CONVERTIBLE_STATUSES = ['MEETING_BOOKED', 'QUALIFIED', 'NO_SHOW', 'SIGNED'];
        if (!CONVERTIBLE_STATUSES.includes(prospect.status)) {
            throw new Error(`Impossible de convertir : statut actuel = ${prospect.status}. Le prospect doit au minimum être qualifié.`);
        }

        // Déjà converti ?
        if (prospect.convertedLeadId) {
            return { leadId: prospect.convertedLeadId, prospect };
        }

        const resolvedServiceId = serviceId || prospect.interestServiceId || 'consultation_juridique';
        const leadId = `LEAD-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;

        // Résoudre le nom lisible et les documents requis depuis SERVICE_CATALOG
        const catalogEntry = SERVICE_CATALOG.find(s => s.id === resolvedServiceId);
        const resolvedServiceName = catalogEntry?.name || resolvedServiceId;

        // Construire la checklist de documents requis
        const requiredDocsList = (catalogEntry?.requiredDocs || []).map(rd => {
            const docInfo = DOCUMENT_CATALOG[rd.docId];
            return {
                id: rd.docId,
                name: docInfo?.name || rd.docId,
                description: docInfo?.description || '',
                category: docInfo?.category || 'OTHER',
                required: rd.required,
            };
        });

        // Préparer les données enrichies depuis le prospect
        const prospectData = {
            qualificationScore: prospect.score,
            source: prospect.source,
            campaignName: prospect.campaignName,
            address: prospect.address,
            city: prospect.city,
            zipCode: prospect.zipCode,
            country: prospect.country,
            callCount: prospect.callLogs.length,
            conversionDate: new Date().toISOString(),
            prospectId: prospect.id,
        };

        // Créer le Lead dans le CRM avec status PAID (paiement déjà confirmé)
        const lead = await this.prisma.lead.create({
            data: {
                id: leadId,
                name: `${prospect.firstName} ${prospect.lastName}`,
                email: prospect.email || `${prospect.phone}@prospect.simulegal.fr`,
                phone: prospect.phone,
                serviceId: resolvedServiceId,
                serviceName: resolvedServiceName,
                status: 'PAID',
                originAgencyId: prospect.agencyId,
                documents: '[]',
                requiredDocs: JSON.stringify(requiredDocsList),
                data: JSON.stringify(prospectData),
            },
        });

        // Transférer les notes du prospect vers le Lead
        const relevantNotes = prospect.notes.filter(n => !n.text.startsWith('[AUTO]'));
        for (const note of relevantNotes) {
            await this.prisma.leadNote.create({
                data: {
                    leadId: lead.id,
                    content: `[Prospect → Sales] ${note.text}`,
                    author: 'HQ',
                },
            });
        }

        // Note de conversion sur le Lead
        await this.prisma.leadNote.create({
            data: {
                leadId: lead.id,
                content: `✅ Lead créé automatiquement depuis le pipeline Sales.\n` +
                    `Prospect: ${prospect.firstName} ${prospect.lastName} (${prospect.id})\n` +
                    `Score: ${prospect.score}/100 | Source: ${prospect.source}\n` +
                    `Appels: ${prospect.callLogs.length} | Adresse: ${prospect.address || 'N/A'}, ${prospect.zipCode || ''} ${prospect.city || ''}`,
                author: 'HQ',
            },
        });

        // Lier le prospect au lead (lien bidirectionnel)
        const updatedProspect = await this.prisma.prospect.update({
            where: { id: prospectId },
            data: {
                status: 'SIGNED',
                convertedLeadId: leadId,
            },
        });

        // Trigger notification automation
        await this.triggerAutomation(updatedProspect, 'SIGNED');

        console.log(`[CONVERSION] ✅ ${prospect.firstName} ${prospect.lastName} → Lead ${leadId} (service: ${resolvedServiceId})`);

        return { leadId: lead.id, prospect: updatedProspect };
    }
}
