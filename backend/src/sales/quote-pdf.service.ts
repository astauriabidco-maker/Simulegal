import { Injectable, Logger } from '@nestjs/common';
import PDFDocument = require('pdfkit');
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class QuotePdfService {
    private readonly logger = new Logger(QuotePdfService.name);

    constructor(private prisma: PrismaService) { }

    /**
     * Génère un devis PDF pour un prospect
     * Inclut : identité du prospect, service envisagé, prix, conditions, validité
     */
    async generateQuotePdf(prospectId: string, options?: {
        serviceId?: string;
        serviceName?: string;
        priceEuros?: number;
        validityDays?: number;
        notes?: string;
    }): Promise<{ buffer: Buffer; filename: string }> {
        const prospect = await this.prisma.prospect.findUnique({
            where: { id: prospectId },
        });

        if (!prospect) {
            throw new Error(`Prospect ${prospectId} introuvable.`);
        }

        const quoteNumber = `DEV-${new Date().getFullYear()}-${prospectId.substring(0, 6).toUpperCase()}`;
        const issueDate = new Date();
        const validityDays = options?.validityDays || 30;
        const expiryDate = new Date(issueDate.getTime() + validityDays * 24 * 60 * 60 * 1000);

        // Le prix est passé en paramètre ou utilise un défaut
        let priceEuros = options?.priceEuros || 150;
        let serviceName = options?.serviceName || 'Prestation juridique';

        const buffer = await new Promise<Buffer>((resolve, reject) => {
            try {
                const doc = new PDFDocument({ margin: 50, size: 'A4' });
                const buffers: Buffer[] = [];

                doc.on('data', buffers.push.bind(buffers));
                doc.on('end', () => resolve(Buffer.concat(buffers)));

                // ═══════════════════════════════════════
                // EN-TÊTE
                // ═══════════════════════════════════════
                doc.fontSize(24).font('Helvetica-Bold').fillColor('#1e293b').text('DEVIS', { align: 'right' });
                doc.fontSize(10).font('Helvetica').fillColor('#64748b').text(`N° ${quoteNumber}`, { align: 'right' });
                doc.moveDown(1.5);

                // Infos Entreprise
                doc.fontSize(12).font('Helvetica-Bold').fillColor('#1e293b').text('SIMULEGAL SAS');
                doc.fontSize(9).font('Helvetica').fillColor('#475569');
                doc.text('Assistance juridique & administrative');
                doc.text('SIRET: 123 456 789 00012');
                doc.text('contact@simulegal.fr');
                doc.moveDown(1);

                // Séparateur
                doc.moveTo(50, doc.y).lineTo(545, doc.y).strokeColor('#e2e8f0').lineWidth(1).stroke();
                doc.moveDown(1);

                // ═══════════════════════════════════════
                // DESTINATAIRE
                // ═══════════════════════════════════════
                doc.fontSize(10).font('Helvetica-Bold').fillColor('#475569').text('DESTINATAIRE :');
                doc.fontSize(11).font('Helvetica-Bold').fillColor('#1e293b').text(`${prospect.firstName} ${prospect.lastName}`);
                doc.fontSize(9).font('Helvetica').fillColor('#475569');
                if (prospect.email) doc.text(prospect.email);
                if (prospect.phone) doc.text(prospect.phone);
                if (prospect.address || prospect.city) {
                    doc.text([prospect.address, prospect.zipCode, prospect.city, prospect.country].filter(Boolean).join(', '));
                }
                doc.moveDown(1);

                // Dates
                doc.fontSize(9).font('Helvetica');
                doc.text(`Date d'émission : ${issueDate.toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' })}`);
                doc.text(`Valide jusqu'au : ${expiryDate.toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' })}`);
                doc.moveDown(2);

                // ═══════════════════════════════════════
                // TABLEAU DES PRESTATIONS
                // ═══════════════════════════════════════
                const tableTop = doc.y;

                // Header du tableau
                doc.rect(50, tableTop, 495, 25).fill('#f1f5f9');
                doc.fontSize(9).font('Helvetica-Bold').fillColor('#334155');
                doc.text('DÉSIGNATION', 60, tableTop + 8);
                doc.text('QTÉ', 360, tableTop + 8, { width: 40, align: 'center' });
                doc.text('P.U. TTC', 405, tableTop + 8, { width: 60, align: 'right' });
                doc.text('TOTAL TTC', 475, tableTop + 8, { width: 65, align: 'right' });

                // Ligne prestation
                const rowY = tableTop + 30;
                doc.fontSize(10).font('Helvetica').fillColor('#1e293b');
                doc.text(serviceName, 60, rowY, { width: 290 });
                doc.text('1', 360, rowY, { width: 40, align: 'center' });
                doc.text(`${priceEuros.toFixed(2)} €`, 405, rowY, { width: 60, align: 'right' });
                doc.text(`${priceEuros.toFixed(2)} €`, 475, rowY, { width: 65, align: 'right' });

                // Séparateur
                doc.moveTo(50, rowY + 20).lineTo(545, rowY + 20).strokeColor('#e2e8f0').stroke();

                // Total
                const totalY = rowY + 35;
                doc.rect(350, totalY - 5, 195, 30).fill('#1e293b');
                doc.fontSize(11).font('Helvetica-Bold').fillColor('#ffffff');
                doc.text('TOTAL TTC :', 360, totalY + 2, { width: 80, align: 'left' });
                doc.text(`${priceEuros.toFixed(2)} €`, 450, totalY + 2, { width: 85, align: 'right' });
                doc.moveDown(3);

                // Facilité de paiement
                doc.y = totalY + 45;
                doc.fontSize(9).font('Helvetica').fillColor('#475569');
                doc.text(`💳 Paiement en 3x sans frais possible : ${(priceEuros / 3).toFixed(2)} €/mois`, 50);
                doc.moveDown(2);

                // ═══════════════════════════════════════
                // NOTES
                // ═══════════════════════════════════════
                if (options?.notes) {
                    doc.fontSize(10).font('Helvetica-Bold').fillColor('#1e293b').text('OBSERVATIONS :');
                    doc.fontSize(9).font('Helvetica').fillColor('#475569').text(options.notes);
                    doc.moveDown(1.5);
                }

                // ═══════════════════════════════════════
                // CONDITIONS
                // ═══════════════════════════════════════
                doc.fontSize(10).font('Helvetica-Bold').fillColor('#1e293b').text('CONDITIONS :');
                doc.fontSize(8).font('Helvetica').fillColor('#94a3b8');
                doc.text('• Ce devis est valable ' + validityDays + ' jours à compter de sa date d\'émission.');
                doc.text('• TVA non applicable, art. 293 B du CGI.');
                doc.text('• Toute prestation commencée est due en totalité.');
                doc.text('• Le paiement vaut acceptation des conditions générales de vente.');
                doc.moveDown(2);

                // Signatures
                const sigY = doc.y;
                doc.fontSize(9).font('Helvetica-Bold').fillColor('#475569');
                doc.text('Pour SIMULEGAL', 50, sigY);
                doc.text('Le Client', 350, sigY);
                doc.fontSize(8).font('Helvetica').fillColor('#94a3b8');
                doc.text('Signature et cachet', 50, sigY + 45);
                doc.text('Bon pour accord, signature', 350, sigY + 45);

                // Ligne de signature
                doc.moveTo(50, sigY + 40).lineTo(200, sigY + 40).strokeColor('#cbd5e1').stroke();
                doc.moveTo(350, sigY + 40).lineTo(500, sigY + 40).strokeColor('#cbd5e1').stroke();

                // Footer
                doc.fontSize(7).font('Helvetica-Oblique').fillColor('#cbd5e1');
                doc.text('SIMULEGAL SAS — Assistance juridique & administrative — www.simulegal.fr', 50, doc.page.height - 50, { align: 'center' });

                doc.end();
            } catch (err) {
                reject(err);
            }
        });

        const filename = `devis_${prospect.lastName.toLowerCase()}_${quoteNumber}.pdf`;
        this.logger.log(`[QuotePDF] 📄 Devis généré: ${filename} pour ${prospect.firstName} ${prospect.lastName}`);

        return { buffer, filename };
    }
}
