"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.FranchiseLeadsService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
const agencies_service_1 = require("../agencies/agencies.service");
const users_service_1 = require("../users/users.service");
const devices_service_1 = require("../devices/devices.service");
let FranchiseLeadsService = class FranchiseLeadsService {
    prisma;
    agenciesService;
    usersService;
    devicesService;
    constructor(prisma, agenciesService, usersService, devicesService) {
        this.prisma = prisma;
        this.agenciesService = agenciesService;
        this.usersService = usersService;
        this.devicesService = devicesService;
    }
    async findAll() {
        return this.prisma.franchiseLead.findMany({
            orderBy: { updatedAt: 'desc' }
        });
    }
    async findOne(id) {
        return this.prisma.franchiseLead.findUnique({
            where: { id },
            include: {
                convertedAgency: true,
                notes: { orderBy: { createdAt: 'desc' } }
            }
        });
    }
    async create(data) {
        return this.prisma.franchiseLead.create({
            data: {
                ...data,
                status: 'NEW'
            }
        });
    }
    async update(id, data) {
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
    async signContract(id) {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead)
            throw new common_1.BadRequestException('Lead not found');
        if (lead.status === 'SIGNED')
            throw new common_1.BadRequestException('Already signed');
        if (lead.status !== 'CONTRACT_SENT') {
            const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'CONTRACT_SENT', 'SIGNED'];
            if (statusOrder.indexOf(lead.status) < statusOrder.indexOf('VALIDATED')) {
                throw new common_1.BadRequestException('Projet non validé.');
            }
        }
        const contract = JSON.parse(lead.contractDetails || '{}');
        const agencyType = contract.type || 'FRANCHISE';
        const agencyName = lead.companyName || lead.name;
        const agencyId = `${agencyName.substring(0, 3).toUpperCase()}-${Date.now().toString().slice(-4)}`;
        const agency = await this.agenciesService.create({
            id: agencyId,
            name: agencyName,
            type: agencyType,
            contactEmail: lead.email,
            region: lead.region,
            zipCodes: '[]',
            commissionRate: contract.commissionRate || (agencyType === 'CORNER' ? 5 : 15),
            kioskUrl: `https://simulegal.fr/kiosk/${agencyId}`
        });
        const password = Math.random().toString(36).slice(-8);
        const user = await this.usersService.create({
            email: lead.email,
            password: password,
            name: `Gérant ${lead.name}`,
            role: 'AGENCY_MANAGER',
            homeAgencyId: agency.id,
            scopeAgencyIds: JSON.stringify([agency.id]),
            permissions: '[]'
        });
        if (agencyType === 'CORNER') {
            await this.devicesService.createProvisioned(agency.id, agency.name);
        }
        const updatedLead = await this.prisma.franchiseLead.update({
            where: { id },
            data: {
                status: 'SIGNED',
                convertedAgencyId: agency.id
            }
        });
        await this.addNote(id, '✅ Contrat signé électroniquement. Agence et accès créés.', 'Système', 'SYSTEM');
        return {
            lead: updatedLead,
            agency,
            user: { ...user, tempPassword: password }
        };
    }
    async generateContract(id) {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead)
            throw new common_1.BadRequestException('Lead not found');
        const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'CONTRACT_SENT', 'SIGNED'];
        const currentStage = statusOrder.indexOf(lead.status);
        const requiredStage = statusOrder.indexOf('VALIDATED');
        if (currentStage < requiredStage) {
            throw new common_1.BadRequestException('Le projet doit être validé (Info Entreprise + Contrat) avant de générer le document.');
        }
        const PDFDocument = require('pdfkit');
        const doc = new PDFDocument();
        const buffers = [];
        doc.on('data', buffers.push.bind(buffers));
        return new Promise((resolve, reject) => {
            doc.on('end', async () => {
                const pdfData = Buffer.concat(buffers);
                await this.prisma.franchiseLeadNote.create({
                    data: {
                        leadId: id,
                        content: '📄 Contrat généré (PDF)',
                        author: 'Système',
                        type: 'SYSTEM'
                    }
                });
                if (lead.status === 'VALIDATED') {
                    await this.prisma.franchiseLead.update({
                        where: { id },
                        data: { status: 'CONTRACT_SENT' }
                    });
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
            }
            else {
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
    async addNote(id, content, author, type = 'NOTE') {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead)
            throw new common_1.BadRequestException('Lead not found');
        return this.prisma.franchiseLeadNote.create({
            data: {
                leadId: id,
                content,
                author,
                type
            }
        });
    }
};
exports.FranchiseLeadsService = FranchiseLeadsService;
exports.FranchiseLeadsService = FranchiseLeadsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        agencies_service_1.AgenciesService,
        users_service_1.UsersService,
        devices_service_1.DevicesService])
], FranchiseLeadsService);
//# sourceMappingURL=franchise-leads.service.js.map