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
const notifications_service_1 = require("../notifications/notifications.service");
let FranchiseLeadsService = class FranchiseLeadsService {
    prisma;
    agenciesService;
    usersService;
    devicesService;
    notificationsService;
    constructor(prisma, agenciesService, usersService, devicesService, notificationsService) {
        this.prisma = prisma;
        this.agenciesService = agenciesService;
        this.usersService = usersService;
        this.devicesService = devicesService;
        this.notificationsService = notificationsService;
    }
    async findAll() {
        const leads = await this.prisma.franchiseLead.findMany({
            orderBy: { updatedAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }
    async findOne(id) {
        const lead = await this.prisma.franchiseLead.findUnique({
            where: { id },
            include: {
                convertedAgency: true,
                notes: { orderBy: { createdAt: 'desc' } }
            }
        });
        if (!lead)
            return null;
        return this.mapLead(lead);
    }
    mapLead(lead) {
        return {
            ...lead,
            contractDetails: lead.contractDetails ? JSON.parse(lead.contractDetails) : {},
            contractHistory: lead.contractHistory ? JSON.parse(lead.contractHistory) : [],
            documents: lead.documents ? JSON.parse(lead.documents) : []
        };
    }
    async create(data) {
        const { contractDetails, contractHistory, documents, ...rest } = data;
        const lead = await this.prisma.franchiseLead.create({
            data: {
                ...rest,
                contractDetails: contractDetails ? JSON.stringify(contractDetails) : "{}",
                contractHistory: contractHistory ? JSON.stringify(contractHistory) : "[]",
                documents: documents ? JSON.stringify(documents) : "[]",
                status: 'NEW'
            }
        });
        return this.mapLead(lead);
    }
    async update(id, data) {
        const updateData = { ...data };
        if (updateData.contractDetails)
            updateData.contractDetails = JSON.stringify(updateData.contractDetails);
        if (updateData.contractHistory)
            updateData.contractHistory = JSON.stringify(updateData.contractHistory);
        if (updateData.documents)
            updateData.documents = JSON.stringify(updateData.documents);
        const lead = await this.prisma.franchiseLead.update({
            where: { id },
            data: updateData
        });
        return this.mapLead(lead);
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
        const contract = lead.contractDetails ? (typeof lead.contractDetails === 'string' ? JSON.parse(lead.contractDetails) : lead.contractDetails) : {};
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
        await this.notificationsService.onFranchiseOnboarding(lead, password);
        await this.addNote(id, '✅ Contrat signé. Agence créée et e-mail de bienvenue envoyé.', 'Système', 'SYSTEM');
        return {
            lead: updatedLead,
            agency,
            user: { ...user, tempPassword: password }
        };
    }
    async updateDocuments(id, documents) {
        return this.update(id, { documents: JSON.stringify(documents) });
    }
    async logContractHistory(id, version) {
        const lead = await this.findOne(id);
        if (!lead)
            throw new common_1.BadRequestException('Lead not found');
        const history = lead.contractHistory || [];
        history.push({
            ...version,
            timestamp: new Date().toISOString()
        });
        return this.update(id, { contractHistory: history });
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
            const contract = lead.contractDetails ? (typeof lead.contractDetails === 'string' ? JSON.parse(lead.contractDetails) : lead.contractDetails) : {};
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
    async getAnalytics() {
        const leads = await this.prisma.franchiseLead.findMany({
            select: { status: true, region: true, createdAt: true }
        });
        const statusCounts = {};
        leads.forEach(l => {
            statusCounts[l.status] = (statusCounts[l.status] || 0) + 1;
        });
        const regionCounts = {};
        leads.forEach(l => {
            regionCounts[l.region] = (regionCounts[l.region] || 0) + 1;
        });
        const totalNew = leads.length;
        const totalSigned = statusCounts['SIGNED'] || 0;
        const conversionRate = totalNew > 0 ? Math.round((totalSigned / totalNew) * 100) : 0;
        const now = new Date();
        const monthlyTrend = [];
        for (let i = 5; i >= 0; i--) {
            const monthStart = new Date(now.getFullYear(), now.getMonth() - i, 1);
            const monthEnd = new Date(now.getFullYear(), now.getMonth() - i + 1, 0);
            const monthLabel = monthStart.toLocaleDateString('fr-FR', { month: 'short', year: '2-digit' });
            const monthLeads = leads.filter(l => {
                const d = new Date(l.createdAt);
                return d >= monthStart && d <= monthEnd;
            });
            monthlyTrend.push({
                month: monthLabel,
                count: monthLeads.length,
                signed: monthLeads.filter(l => l.status === 'SIGNED').length
            });
        }
        return {
            total: leads.length,
            statusCounts,
            regionCounts,
            conversionRate,
            monthlyTrend
        };
    }
    async exportToCSV(filters) {
        let where = {};
        if (filters?.region)
            where.region = filters.region;
        if (filters?.status)
            where.status = filters.status;
        const leads = await this.prisma.franchiseLead.findMany({
            where,
            orderBy: { createdAt: 'desc' }
        });
        const headers = ['ID', 'Nom', 'Email', 'Téléphone', 'Ville', 'Région', 'Statut', 'Société', 'SIRET', 'Date Création'];
        const rows = leads.map(l => [
            l.id,
            l.name,
            l.email,
            l.phone,
            l.targetCity,
            l.region,
            l.status,
            l.companyName || '',
            l.siret || '',
            new Date(l.createdAt).toLocaleDateString('fr-FR')
        ].map(v => `"${(v || '').toString().replace(/"/g, '""')}"`).join(';'));
        return [headers.join(';'), ...rows].join('\n');
    }
};
exports.FranchiseLeadsService = FranchiseLeadsService;
exports.FranchiseLeadsService = FranchiseLeadsService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService,
        agencies_service_1.AgenciesService,
        users_service_1.UsersService,
        devices_service_1.DevicesService,
        notifications_service_1.NotificationsService])
], FranchiseLeadsService);
//# sourceMappingURL=franchise-leads.service.js.map