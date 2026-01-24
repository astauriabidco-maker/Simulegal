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
exports.InvoicesService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let InvoicesService = class InvoicesService {
    prisma;
    constructor(prisma) {
        this.prisma = prisma;
    }
    async getInvoiceData(leadId) {
        const lead = await this.prisma.lead.findUnique({
            where: { id: leadId },
            include: { originAgency: true }
        });
        if (!lead)
            return null;
        return {
            invoiceNumber: lead.invoiceNumber,
            date: lead.paymentDate || lead.updatedAt,
            client: {
                name: lead.name,
                email: lead.email,
                phone: lead.phone
            },
            service: {
                name: lead.serviceName,
                amount: lead.amountPaid
            },
            agency: lead.originAgency ? {
                name: lead.originAgency.name,
                city: lead.originAgency.city
            } : {
                name: 'SimuLegal HQ',
                city: 'Paris'
            },
            payment: {
                method: lead.paymentMethod,
                reference: lead.paymentRef
            }
        };
    }
    async generatePdf(leadId) {
        const data = await this.getInvoiceData(leadId);
        if (!data)
            return null;
        console.log(`[INVOICE] 📄 PDF Generated for ${data.invoiceNumber}`);
        return {
            filename: `${data.invoiceNumber}.pdf`,
            content: `SIMULEGAL INVOICE\nRef: ${data.invoiceNumber}\nClient: ${data.client.name}\nAmount: ${data.service.amount / 100}€`
        };
    }
};
exports.InvoicesService = InvoicesService;
exports.InvoicesService = InvoicesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], InvoicesService);
//# sourceMappingURL=invoices.service.js.map