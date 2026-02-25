import { Injectable, Logger } from '@nestjs/common';
import PDFDocument = require('pdfkit');
import { PrismaService } from '../prisma/prisma.service';
import { DOCUMENT_CATALOG } from '../config/services-pipeline.config';

interface ChecklistDoc {
    id: string;
    name: string;
    label?: string;
    description?: string;
    category?: string;
    required?: boolean;
}

@Injectable()
export class ChecklistPdfService {
    private readonly logger = new Logger(ChecklistPdfService.name);

    constructor(private prisma: PrismaService) { }

    /**
     * Génère un PDF de checklist personnalisée pour un lead.
     * Contient la liste de tous les documents requis, groupés par catégorie,
     * avec des cases à cocher pour que le client puisse les préparer.
     */
    async generateChecklistPdf(leadId: string): Promise<Buffer> {
        const lead = await this.prisma.lead.findUnique({
            where: { id: leadId },
        });

        if (!lead) {
            throw new Error(`Lead ${leadId} introuvable.`);
        }

        // Parse requiredDocs from lead
        let requiredDocs: ChecklistDoc[] = [];
        try {
            const raw = typeof lead.requiredDocs === 'string'
                ? JSON.parse(lead.requiredDocs)
                : lead.requiredDocs;

            if (Array.isArray(raw)) {
                requiredDocs = raw.map((doc: any) => {
                    // Enrichir avec le DOCUMENT_CATALOG si possible
                    const catalogDoc = DOCUMENT_CATALOG[doc.id || doc.docId];
                    return {
                        id: doc.id || doc.docId || 'unknown',
                        name: doc.name || doc.label || catalogDoc?.name || doc.id || 'Document',
                        description: doc.description || catalogDoc?.description || '',
                        category: doc.category || catalogDoc?.category || 'OTHER',
                        required: doc.required !== false,
                    };
                });
            }
        } catch (e) {
            this.logger.warn(`[ChecklistPdf] Could not parse requiredDocs for lead ${leadId}`);
        }

        if (requiredDocs.length === 0) {
            throw new Error(`Aucun document requis pour le dossier ${leadId}.`);
        }

        // Group by category
        const categoryLabels: Record<string, string> = {
            'IDENTITY': '🪪 Pièces d\'identité',
            'CIVIL': '📋 État civil',
            'RESIDENCE': '🏠 Résidence / Domicile',
            'FINANCIAL': '💰 Ressources financières',
            'PROFESSIONAL': '💼 Professionnel',
            'EDUCATION': '🎓 Formation / Études',
            'HEALTH': '🏥 Santé',
            'OTHER': '📎 Autres documents',
        };

        const grouped: Record<string, ChecklistDoc[]> = {};
        for (const doc of requiredDocs) {
            const cat = doc.category || 'OTHER';
            if (!grouped[cat]) grouped[cat] = [];
            grouped[cat].push(doc);
        }

        // Category display order
        const categoryOrder = ['IDENTITY', 'CIVIL', 'RESIDENCE', 'FINANCIAL', 'PROFESSIONAL', 'EDUCATION', 'HEALTH', 'OTHER'];

        return new Promise((resolve, reject) => {
            try {
                const doc = new PDFDocument({ margin: 50, size: 'A4' });
                const buffers: Buffer[] = [];

                doc.on('data', buffers.push.bind(buffers));
                doc.on('end', () => resolve(Buffer.concat(buffers)));

                // ═══════════════════════════════════════
                // EN-TÊTE
                // ═══════════════════════════════════════
                doc.fontSize(22).font('Helvetica-Bold').text('CHECKLIST DOCUMENTS', { align: 'center' });
                doc.moveDown(0.3);
                doc.fontSize(10).font('Helvetica').fillColor('#666666')
                    .text('Liste personnalisée des pièces justificatives à fournir', { align: 'center' });
                doc.moveDown(0.5);

                // Ligne de séparation
                doc.moveTo(50, doc.y).lineTo(545, doc.y).strokeColor('#4F46E5').lineWidth(2).stroke();
                doc.moveDown(0.8);

                // ═══════════════════════════════════════
                // INFOS CLIENT
                // ═══════════════════════════════════════
                doc.fillColor('#000000');
                doc.fontSize(10).font('Helvetica-Bold').text('Dossier de : ', { continued: true });
                doc.font('Helvetica').text(lead.name || 'N/A');

                doc.font('Helvetica-Bold').text('Service : ', { continued: true });
                doc.font('Helvetica').text(lead.serviceName || 'N/A');

                doc.font('Helvetica-Bold').text('Référence : ', { continued: true });
                doc.font('Helvetica').text(lead.invoiceNumber || lead.id);

                doc.font('Helvetica-Bold').text('Date : ', { continued: true });
                doc.font('Helvetica').text(new Date().toLocaleDateString('fr-FR', {
                    weekday: 'long', day: 'numeric', month: 'long', year: 'numeric'
                }));

                doc.moveDown(1);

                // ═══════════════════════════════════════
                // INSTRUCTIONS
                // ═══════════════════════════════════════
                const instructionY = doc.y;
                doc.rect(50, instructionY, 495, 60).fillColor('#F5F3FF').fill();
                doc.fillColor('#4F46E5').fontSize(9).font('Helvetica-Bold');
                doc.text('📌 INSTRUCTIONS', 60, instructionY + 8);
                doc.fillColor('#374151').font('Helvetica').fontSize(8);
                doc.text(
                    '• Cochez chaque document une fois préparé\n' +
                    '• Tous les documents marqués (*) sont obligatoires\n' +
                    '• Les documents doivent être scannés en format PDF ou photo (JPEG/PNG)\n' +
                    '• Déposez vos documents via votre espace client sécurisé : simulegal.fr/client',
                    60, instructionY + 22, { width: 475, lineGap: 2 }
                );
                doc.y = instructionY + 68;
                doc.moveDown(0.8);

                // ═══════════════════════════════════════
                // CHECKLIST GROUPÉE PAR CATÉGORIE
                // ═══════════════════════════════════════
                let docIndex = 1;

                for (const cat of categoryOrder) {
                    const docs = grouped[cat];
                    if (!docs || docs.length === 0) continue;

                    // Check if we need a new page
                    if (doc.y > 700) {
                        doc.addPage();
                    }

                    // Category header
                    const catHeaderY = doc.y;
                    doc.rect(50, catHeaderY, 495, 20).fillColor('#EEF2FF').fill();
                    doc.fillColor('#4338CA').fontSize(10).font('Helvetica-Bold');
                    doc.text(categoryLabels[cat] || cat, 58, catHeaderY + 5);
                    doc.y = catHeaderY + 26;

                    // Documents in this category
                    for (const item of docs) {
                        if (doc.y > 750) {
                            doc.addPage();
                        }

                        const itemY = doc.y;

                        // Checkbox (empty square)
                        doc.rect(58, itemY + 1, 12, 12).strokeColor('#9CA3AF').lineWidth(1).stroke();

                        // Document number + name
                        doc.fillColor('#111827').fontSize(10).font('Helvetica-Bold');
                        const requiredMarker = item.required ? ' *' : '';
                        doc.text(`${docIndex}. ${item.name}${requiredMarker}`, 78, itemY + 1, { width: 410 });

                        // Description
                        if (item.description) {
                            doc.fillColor('#6B7280').fontSize(8).font('Helvetica');
                            doc.text(`    ${item.description}`, 78, doc.y + 1, { width: 410 });
                        }

                        doc.moveDown(0.6);
                        docIndex++;
                    }

                    doc.moveDown(0.4);
                }

                // ═══════════════════════════════════════
                // RÉCAPITULATIF
                // ═══════════════════════════════════════
                if (doc.y > 700) doc.addPage();

                doc.moveDown(0.5);
                doc.moveTo(50, doc.y).lineTo(545, doc.y).strokeColor('#E5E7EB').lineWidth(1).stroke();
                doc.moveDown(0.5);

                const totalDocs = requiredDocs.length;
                const mandatoryDocs = requiredDocs.filter(d => d.required).length;
                const optionalDocs = totalDocs - mandatoryDocs;

                doc.fillColor('#111827').fontSize(10).font('Helvetica-Bold');
                doc.text(`Total : ${totalDocs} document(s)`, 50);
                doc.fillColor('#4F46E5').fontSize(9).font('Helvetica');
                doc.text(`  • ${mandatoryDocs} obligatoire(s) *`);
                if (optionalDocs > 0) {
                    doc.fillColor('#6B7280');
                    doc.text(`  • ${optionalDocs} optionnel(s)`);
                }

                // ═══════════════════════════════════════
                // FOOTER
                // ═══════════════════════════════════════
                doc.fontSize(8).font('Helvetica-Oblique').fillColor('#9CA3AF');
                doc.text(
                    'Document généré automatiquement par SimuLegal. En cas de doute sur un document, contactez votre juriste.',
                    50, doc.page.height - 80,
                    { align: 'center', width: 495 }
                );
                doc.text(
                    `📧 contact@simulegal.fr | 📞 01 23 45 67 89 | 🌐 simulegal.fr`,
                    50, doc.page.height - 65,
                    { align: 'center', width: 495 }
                );

                doc.end();
            } catch (err) {
                reject(err);
            }
        });
    }
}
