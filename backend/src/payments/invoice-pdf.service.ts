import { Injectable, Logger } from '@nestjs/common';
import PDFDocument = require('pdfkit');
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class InvoicePdfService {
    private readonly logger = new Logger(InvoicePdfService.name);

    constructor(private prisma: PrismaService) { }

    async generateInvoicePdf(leadId: string): Promise<Buffer> {
        const lead = await this.prisma.lead.findUnique({
            where: { id: leadId },
            include: { transactions: true }
        });

        if (!lead) {
            throw new Error(`Lead ${leadId} introuvable.`);
        }

        const invoiceNumber = lead.invoiceNumber || `FAC-${new Date().getFullYear()}-${leadId.substring(0, 6).toUpperCase()}`;
        const issueDate = lead.paymentDate ? new Date(lead.paymentDate) : new Date();

        return new Promise((resolve, reject) => {
            try {
                const doc = new PDFDocument({ margin: 50 });
                const buffers: Buffer[] = [];

                doc.on('data', buffers.push.bind(buffers));
                doc.on('end', () => resolve(Buffer.concat(buffers)));

                // --- En-tête ---
                doc.fontSize(20).text('FACTURE', { align: 'right' });
                doc.moveDown();

                // Infos Entreprise (Simulegal)
                doc.fontSize(10).font('Helvetica-Bold').text('SIMULEGAL SAS');
                doc.font('Helvetica').text('123 Avenue des Droits');
                doc.text('75001 Paris, FRANCE');
                doc.text('SIRET: 123 456 789 00012');
                doc.text('TVA Intra: FR 12 123456789');
                doc.moveDown();

                // Infos Client
                doc.font('Helvetica-Bold').text('Facturé à :');
                doc.font('Helvetica').text(lead.name);
                doc.text(lead.email);
                if (lead.phone) doc.text(lead.phone);
                doc.moveDown(2);

                // Détails Facture
                doc.font('Helvetica-Bold').text(`Numéro de facture : `, { continued: true }).font('Helvetica').text(invoiceNumber);
                doc.font('Helvetica-Bold').text(`Date d'émission : `, { continued: true }).font('Helvetica').text(issueDate.toLocaleDateString('fr-FR'));
                if (lead.paymentRef) {
                    doc.font('Helvetica-Bold').text(`Référence de paiement : `, { continued: true }).font('Helvetica').text(lead.paymentRef);
                }
                doc.moveDown(2);

                // Tableau des prestations
                const tableTop = doc.y;
                doc.font('Helvetica-Bold');
                doc.text('Description', 50, tableTop);
                doc.text('Montant TTC', 400, tableTop, { width: 100, align: 'right' });
                doc.moveTo(50, tableTop + 15).lineTo(500, tableTop + 15).stroke();

                doc.moveDown(0.5);
                const itemY = doc.y;
                doc.font('Helvetica');
                doc.text(`Prestation juridique : ${lead.serviceName}`, 50, itemY);

                const amountEuros = (lead.amountPaid || 0) / 100;
                doc.text(`${amountEuros.toFixed(2)} €`, 400, itemY, { width: 100, align: 'right' });

                doc.moveTo(50, itemY + 20).lineTo(500, itemY + 20).stroke();
                doc.moveDown(2);

                // Total
                const totalY = doc.y + 15;
                doc.font('Helvetica-Bold').text('TOTAL RÉGLÉ :', 250, totalY, { width: 150, align: 'right' });
                doc.text(`${amountEuros.toFixed(2)} €`, 400, totalY, { width: 100, align: 'right' });

                // Footer
                doc.font('Helvetica-Oblique').fontSize(9);
                doc.text('Cette facture a été acquittée et ne requiert aucun paiement supplémentaire.', 50, doc.page.height - 100, { align: 'center' });
                doc.text('TVA non applicable, art. 293 B du CGI.', 50, doc.page.height - 85, { align: 'center' });

                doc.end();
            } catch (err) {
                reject(err);
            }
        });
    }
}
