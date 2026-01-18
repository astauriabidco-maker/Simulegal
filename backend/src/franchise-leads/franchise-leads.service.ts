import { Injectable, BadRequestException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { AgenciesService } from '../agencies/agencies.service';
import { UsersService } from '../users/users.service';
import { DevicesService } from '../devices/devices.service';
import { AgencyType, UserRole, FranchiseLeadStatus } from '@prisma/client';

@Injectable()
export class FranchiseLeadsService {
    constructor(
        private prisma: PrismaService,
        private agenciesService: AgenciesService,
        private usersService: UsersService,
        private devicesService: DevicesService
    ) { }

    async findAll() {
        return this.prisma.franchiseLead.findMany({
            orderBy: { updatedAt: 'desc' }
        });
    }

    async findOne(id: string) {
        return this.prisma.franchiseLead.findUnique({
            where: { id },
            include: {
                convertedAgency: true,
                notes: { orderBy: { createdAt: 'desc' } }
            }
        });
    }

    async create(data: any) {
        return this.prisma.franchiseLead.create({
            data: {
                ...data,
                status: 'NEW'
            }
        });
    }

    async update(id: string, data: any) {
        // Auto-Log Status Change
        if (data.status) {
            const current = await this.prisma.franchiseLead.findUnique({ where: { id }, select: { status: true } });
            if (current && current.status !== data.status) {
                await this.prisma.franchiseLeadNote.create({
                    data: {
                        leadId: id,
                        content: `🔄 Statut modifié : ${current.status} ➔ ${data.status}`,
                        author: 'Système',
                        type: 'SYSTEM'
                    }
                });
            }
        }

        return this.prisma.franchiseLead.update({
            where: { id },
            data
        });
    }

    async signContract(id: string) {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');
        if (lead.status === 'SIGNED') throw new BadRequestException('Already signed');

        // Gate: Contract must be sent
        if (lead.status !== 'CONTRACT_SENT') {
            // Optional: Allow bypassing if we want, but strict mode says no. 
            // Let's be strict but realistic -> Maybe they printed it manually?
            // For now, strict:
            // throw new BadRequestException('Le contrat doit être généré/envoyé avant signature.');
            // Actually, let's just check it's at least VALIDATED.
            const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'CONTRACT_SENT', 'SIGNED'];
            if (statusOrder.indexOf(lead.status) < statusOrder.indexOf('VALIDATED')) {
                throw new BadRequestException('Projet non validé.');
            }
        }

        // 1. Lire les détails du contrat
        const contract = JSON.parse(lead.contractDetails || '{}');
        const agencyType = contract.type || 'FRANCHISE';

        // 2. Créer l'agence
        // On génère un ID unique pour l'agence basé sur le nom ou aléatoire
        const agencyName = lead.companyName || lead.name;
        const agencyId = `${agencyName.substring(0, 3).toUpperCase()}-${Date.now().toString().slice(-4)}`;

        const agency = await this.agenciesService.create({
            id: agencyId,
            name: agencyName, // Use Company Name for the Agency
            type: agencyType,
            contactEmail: lead.email,
            region: lead.region,
            zipCodes: '[]', // À configurer plus tard
            commissionRate: contract.commissionRate || (agencyType === 'CORNER' ? 5 : 15),
            kioskUrl: `https://simulegal.fr/kiosk/${agencyId}`
        });

        // 3. Créer le compte Utilisateur (Gérant)
        const password = Math.random().toString(36).slice(-8); // Mot de passe temporaire
        const user = await this.usersService.create({
            email: lead.email,
            password: password,
            name: `Gérant ${lead.name}`,
            role: 'AGENCY_MANAGER',
            homeAgencyId: agency.id,
            scopeAgencyIds: JSON.stringify([agency.id]),
            permissions: '[]'
        });

        // 4. Si Corner, provisionner la borne
        if (agencyType === 'CORNER') {
            await this.devicesService.createProvisioned(agency.id, agency.name);
        }

        // 5. Mettre à jour le statut du lead
        const updatedLead = await this.prisma.franchiseLead.update({
            where: { id },
            data: {
                status: 'SIGNED',
                convertedAgencyId: agency.id
            }
        });

        // Log System Action
        await this.addNote(id, '✅ Contrat signé électroniquement. Agence et accès créés.', 'Système', 'SYSTEM' as any);

        return {
            lead: updatedLead,
            agency,
            user: { ...user, tempPassword: password }
        };
    }

    async generateContract(id: string): Promise<Buffer> {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');

        // Gate: Project must be validated
        const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'CONTRACT_SENT', 'SIGNED'];
        const currentStage = statusOrder.indexOf(lead.status);
        const requiredStage = statusOrder.indexOf('VALIDATED');

        if (currentStage < requiredStage) {
            throw new BadRequestException('Le projet doit être validé (Info Entreprise + Contrat) avant de générer le document.');
        }

        const PDFDocument = require('pdfkit');
        const doc = new PDFDocument();
        const buffers: Buffer[] = [];

        doc.on('data', buffers.push.bind(buffers));

        return new Promise((resolve, reject) => {
            doc.on('end', async () => {
                const pdfData = Buffer.concat(buffers);

                // Log Generation
                await this.prisma.franchiseLeadNote.create({
                    data: {
                        leadId: id,
                        content: '📄 Contrat généré (PDF)',
                        author: 'Système',
                        type: 'SYSTEM'
                    }
                });

                // Auto-advance status if not yet sent
                if (lead.status === 'VALIDATED') {
                    await this.prisma.franchiseLead.update({
                        where: { id },
                        data: { status: 'CONTRACT_SENT' }
                    });
                    // Log automated transition
                    await this.prisma.franchiseLeadNote.create({
                        data: {
                            leadId: id,
                            content: '🚀 Statut mis à jour automatiquement : CONTRACT_SENT',
                            author: 'Système',
                            type: 'SYSTEM'
                        }
                    });
                }

                resolve(pdfData);
            });

            // --- Contrat Content ---
            doc.fontSize(20).text('CONTRAT DE PARTENARIAT SIMULEGAL', { align: 'center' });
            doc.moveDown();

            const contract = JSON.parse(lead.contractDetails || '{}');
            const typeLabel = contract.type === 'CORNER' ? 'Contrat Corner' : 'Contrat de Franchise';

            doc.fontSize(12).text(`Type de contrat : ${typeLabel}`, { align: 'center' });
            doc.moveDown(2);

            doc.fontSize(14).text('ENTRE LES SOUSSIGNÉS :', { underline: true });
            doc.fontSize(12).text('La société SIMULEGAL HQ, SAS au capital de 10.000€, dont le siège social est situé à Paris.');
            doc.text('Ci-après dénommée "Le Franchiseur"');
            doc.moveDown();

            doc.fontSize(14).text('ET :', { underline: true });
            if (lead.companyName) {
                doc.fontSize(12).text(`La société ${lead.companyName}, forme ${lead.legalForm || 'Non définie'}, immatriculée sous le SIRET ${lead.siret || 'En cours'}.`);
                doc.text(`Représentée par M./Mme ${lead.name}.`);
            } else {
                doc.fontSize(12).text(`M./Mme ${lead.name}, agissant en tant qu'entrepreneur individuel.`);
            }
            doc.text(`Demeurant à : ${lead.targetCity} (${lead.region})`);
            doc.text('Ci-après dénommée "Le Partenaire"');
            doc.moveDown(2);

            doc.fontSize(14).text('IL A ÉTÉ CONVENU CE QUI SUIT :', { underline: true });
            doc.moveDown();

            doc.fontSize(12).text('ARTICLE 1 - OBJET');
            doc.text('Le présent contrat a pour objet de définir les conditions de collaboration entre les parties.');
            doc.moveDown();

            doc.fontSize(12).text('ARTICLE 2 - COMMISSION');
            doc.text(`Le Partenaire percevra une commission de ${contract.commissionRate || 15}% sur le Chiffre d'Affaires généré.`);
            doc.moveDown();

            doc.fontSize(12).text('ARTICLE 3 - DURÉE');
            doc.text('Le contrat est conclu pour une durée indéterminée.');
            doc.moveDown(2);

            doc.text(`Fait à Paris, le ${new Date().toLocaleDateString('fr-FR')}`);
            doc.moveDown(4);

            doc.text('Le Franchiseur', { align: 'left' });
            doc.text('Le Partenaire', { align: 'right' });

            doc.end();
        });
    }

    async addNote(id: string, content: string, author: string, type: 'NOTE' | 'CALL' | 'EMAIL' = 'NOTE') {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');

        return this.prisma.franchiseLeadNote.create({
            data: {
                leadId: id,
                content,
                author,
                type
            }
        });
    }
}
