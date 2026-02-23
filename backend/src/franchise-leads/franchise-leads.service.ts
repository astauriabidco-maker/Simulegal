import { Injectable, BadRequestException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { AgenciesService } from '../agencies/agencies.service';
import { UsersService } from '../users/users.service';
import { DevicesService } from '../devices/devices.service';
import { NotificationsService } from '../notifications/notifications.service';
import { AgencyType, UserRole, FranchiseLeadStatus } from '@prisma/client';
import * as crypto from 'crypto';

/** Génère un mot de passe temporaire sécurisé (12 chars, mix majuscules/minuscules/chiffres/symboles) */
function generateSecurePassword(): string {
    const chars = 'ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789!@#$%';
    const bytes = crypto.randomBytes(12);
    return Array.from(bytes).map(b => chars[b % chars.length]).join('');
}
/** Délai légal Loi Doubin en jours (Art. L330-3 al. 2) */
const COOLING_PERIOD_DAYS = 20;

@Injectable()
export class FranchiseLeadsService {
    constructor(
        private prisma: PrismaService,
        private agenciesService: AgenciesService,
        private usersService: UsersService,
        private devicesService: DevicesService,
        private notificationsService: NotificationsService
    ) { }

    async findAll() {
        const leads = await this.prisma.franchiseLead.findMany({
            orderBy: { updatedAt: 'desc' }
        });
        return leads.map(l => this.mapLead(l));
    }

    async findOne(id: string) {
        const lead = await this.prisma.franchiseLead.findUnique({
            where: { id },
            include: {
                convertedAgency: true,
                notes: { orderBy: { createdAt: 'desc' } }
            }
        });
        if (!lead) return null;
        return this.mapLead(lead);
    }

    private mapLead(lead: any) {
        return {
            ...lead,
            contractDetails: lead.contractDetails ? JSON.parse(lead.contractDetails) : {},
            contractHistory: lead.contractHistory ? JSON.parse(lead.contractHistory) : [],
            documents: lead.documents ? JSON.parse(lead.documents) : [],
            // Calcul dynamique du délai restant
            coolingPeriodRemaining: lead.dipSentAt
                ? Math.max(0, COOLING_PERIOD_DAYS - Math.floor((Date.now() - new Date(lead.dipSentAt).getTime()) / (1000 * 60 * 60 * 24)))
                : null
        };
    }

    async create(data: any) {
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

    async update(id: string, data: any) {
        const updateData = { ...data };
        if (updateData.contractDetails) updateData.contractDetails = JSON.stringify(updateData.contractDetails);
        if (updateData.contractHistory) updateData.contractHistory = JSON.stringify(updateData.contractHistory);
        if (updateData.documents) updateData.documents = JSON.stringify(updateData.documents);

        const lead = await this.prisma.franchiseLead.update({
            where: { id },
            data: updateData
        });

        return this.mapLead(lead);
    }

    // ========================================================
    // LOI DOUBIN — DIP (Document d'Information Précontractuelle)
    // Art. L330-3 & R330-1 du Code de Commerce
    // ========================================================

    /**
     * Envoie le DIP et démarre le délai de réflexion de 20 jours.
     * Le statut passe de VALIDATED → DIP_SENT.
     */
    async sendDIP(id: string) {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');

        const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'DIP_SENT', 'CONTRACT_SENT', 'SIGNED'];
        if (statusOrder.indexOf(lead.status) < statusOrder.indexOf('VALIDATED')) {
            throw new BadRequestException('Le projet doit être validé avant d\'envoyer le DIP.');
        }

        if (lead.status === 'DIP_SENT' && lead.dipSentAt) {
            throw new BadRequestException('Le DIP a déjà été envoyé le ' + new Date(lead.dipSentAt).toLocaleDateString('fr-FR'));
        }

        const now = new Date();
        const updatedLead = await this.prisma.franchiseLead.update({
            where: { id },
            data: {
                status: 'DIP_SENT',
                dipSentAt: now
            }
        });

        await this.addNote(id, `📋 DIP (Document d'Information Précontractuelle) envoyé conformément à l'Art. L330-3. Délai de réflexion de ${COOLING_PERIOD_DAYS} jours démarré.`, 'Système', 'SYSTEM' as any);

        return this.mapLead(updatedLead);
    }

    /**
     * Génère le DIP au format PDF — Art. R330-1 du Code de Commerce
     */
    async generateDIP(id: string): Promise<Buffer> {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');

        const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'DIP_SENT', 'CONTRACT_SENT', 'SIGNED'];
        if (statusOrder.indexOf(lead.status) < statusOrder.indexOf('VALIDATED')) {
            throw new BadRequestException('Le projet doit être validé avant de générer le DIP.');
        }

        const contract = lead.contractDetails ? JSON.parse(lead.contractDetails) : {};
        const PDFDocument = require('pdfkit');
        const doc = new PDFDocument({ margin: 50 });
        const buffers: Buffer[] = [];
        doc.on('data', buffers.push.bind(buffers));

        return new Promise((resolve) => {
            doc.on('end', async () => {
                const pdfData = Buffer.concat(buffers);
                await this.addNote(id, '📋 DIP généré (PDF) — Conforme Art. R330-1', 'Système', 'SYSTEM' as any);
                resolve(pdfData);
            });

            // === HEADER ===
            doc.fontSize(18).text('DOCUMENT D\'INFORMATION PRÉCONTRACTUELLE', { align: 'center' });
            doc.fontSize(10).text('Conformément aux articles L330-3 et R330-1 du Code de Commerce', { align: 'center' });
            doc.moveDown(2);

            // === 1. IDENTIFICATION DU FRANCHISEUR (Art. R330-1, 1°) ===
            doc.fontSize(14).text('1. IDENTIFICATION DU FRANCHISEUR', { underline: true });
            doc.fontSize(10)
                .text('Dénomination : SIMULEGAL HQ')
                .text('Forme juridique : SAS au capital de 10.000€')
                .text('Siège social : 8 Rue de la Paix, 75002 Paris')
                .text('RCS : Paris B 123 456 789')
                .text('Dirigeant : [À compléter]')
                .text('Date de création : [À compléter]')
                .text('Domiciliation bancaire : [À compléter]');
            doc.moveDown();
            doc.text('Les comptes annuels des deux derniers exercices sont joints en annexe du présent document.', { italic: true });
            doc.moveDown(2);

            // === 2. PRÉSENTATION DU RÉSEAU (Art. R330-1, 2°) ===
            doc.fontSize(14).text('2. PRÉSENTATION DU RÉSEAU', { underline: true });
            doc.fontSize(10)
                .text('Marque : SIMULEGAL')
                .text('Nature de l\'activité : Services juridiques d\'aide à l\'éligibilité et accompagnement administratif')
                .text('Historique de l\'enseigne : [À compléter]')
                .text('Liste des membres du réseau : [Jointe en annexe]')
                .text('Nombre total de franchisés au réseau : [À compléter]')
                .text('Nombre de contrats résiliés / non renouvelés au cours des 12 derniers mois : [À compléter]');
            doc.moveDown(2);

            // === 3. ÉTAT DU MARCHÉ LOCAL (Art. R330-1, 2°) ===
            doc.fontSize(14).text('3. ÉTAT DU MARCHÉ LOCAL', { underline: true });
            doc.fontSize(10)
                .text(`Zone d'implantation prévue : ${lead.targetCity} (${lead.region})`)
                .text('État général du marché : [Étude de marché jointe en annexe]')
                .text('Perspectives de développement : [À compléter]');
            doc.moveDown(2);

            // === 4. CONDITIONS FINANCIÈRES (Art. R330-1, 3°) ===
            doc.fontSize(14).text('4. CONDITIONS FINANCIÈRES', { underline: true });
            const entryFeeEuros = lead.entryFee ? (lead.entryFee / 100).toFixed(2) : '[À définir]';
            doc.fontSize(10)
                .text(`Droit d'entrée : ${entryFeeEuros} € HT`)
                .text(`Redevance périodique : ${lead.royaltyRate ?? contract.commissionRate ?? '[À définir]'}% du CA HT`)
                .text(`Contribution fonds publicité : ${lead.advertisingFee ?? '[À définir]'}% du CA HT`)
                .text(`Investissement initial estimé : [À compléter]`)
                .text(`Chiffre d'affaires prévisionnel : voir projections en annexe`);
            doc.moveDown(2);

            // === 5. DURÉE ET CONDITIONS DE RENOUVELLEMENT (Art. R330-1, 4°) ===
            doc.fontSize(14).text('5. DURÉE DU CONTRAT', { underline: true });
            const durationText = lead.contractDuration ? `${lead.contractDuration} mois` : 'Durée indéterminée';
            doc.fontSize(10)
                .text(`Durée : ${durationText}`)
                .text(`Conditions de renouvellement : ${lead.renewalTerms || 'Renouvellement par tacite reconduction, sauf dénonciation par l\'une des parties avec un préavis de ' + (lead.terminationNotice || 3) + ' mois.'}`);
            doc.moveDown(2);

            // === 6. EXCLUSIVITÉ TERRITORIALE (Art. R330-1, 5°) ===
            doc.fontSize(14).text('6. EXCLUSIVITÉ TERRITORIALE', { underline: true });
            if (lead.exclusiveTerritory) {
                doc.fontSize(10)
                    .text(`Le franchisé bénéficie d'une exclusivité territoriale dans un rayon de ${lead.exclusiveRadius || 15} km autour de son point de vente.`)
                    .text('Pendant la durée du contrat, le franchiseur s\'engage à ne pas ouvrir ni autoriser l\'ouverture d\'un autre point de vente sous la même enseigne dans cette zone.');
            } else {
                doc.fontSize(10)
                    .text('Le présent contrat ne prévoit pas d\'exclusivité territoriale au profit du franchisé.');
            }
            doc.moveDown(2);

            // === 7. CONDITIONS DE RÉSILIATION (Art. R330-1, 6°) ===
            doc.fontSize(14).text('7. CONDITIONS DE RÉSILIATION ET DE CESSION', { underline: true });
            doc.fontSize(10)
                .text(`Préavis de résiliation : ${lead.terminationNotice || 3} mois`)
                .text('Conditions de cession du contrat : Soumise à l\'agrément préalable du franchiseur.')
                .text(`Clause de non-concurrence post-contractuelle : ${lead.nonCompeteDuration ? lead.nonCompeteDuration + ' mois après la fin du contrat, dans un rayon de ' + (lead.exclusiveRadius || 15) + ' km.' : 'Aucune.'}`);
            doc.moveDown(2);

            // === 8. DÉLAI DE RÉFLEXION ===
            doc.fontSize(14).text('8. DÉLAI DE RÉFLEXION LÉGAL', { underline: true });
            doc.fontSize(10)
                .text(`Conformément à l'article L330-3 alinéa 2 du Code de commerce, le candidat franchisé dispose d'un délai de ${COOLING_PERIOD_DAYS} jours calendaires à compter de la remise du présent document pour prendre sa décision.`)
                .text('Aucune somme ni aucun engagement ne peut être exigé avant l\'expiration de ce délai.', { bold: true });
            doc.moveDown(2);

            // === SIGNATURES ===
            doc.text(`Fait à Paris, le ${new Date().toLocaleDateString('fr-FR')}`);
            doc.moveDown();
            doc.text(`Remis à : ${lead.name}`);
            doc.moveDown(3);
            doc.text('Le Franchiseur                                              Le Candidat');
            doc.moveDown();
            doc.text('(Signature précédée de la mention "Lu et approuvé")', { italic: true });

            doc.end();
        });
    }

    // ========================================================
    // CONTRAT DE FRANCHISE — Enrichi (Loi Doubin conforme)
    // ========================================================

    async generateContract(id: string): Promise<Buffer> {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');

        const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'DIP_SENT', 'CONTRACT_SENT', 'SIGNED'];
        const currentStage = statusOrder.indexOf(lead.status);

        // Gate: DIP must have been sent first
        if (currentStage < statusOrder.indexOf('DIP_SENT')) {
            throw new BadRequestException('Le DIP doit être envoyé avant de générer le contrat (Art. L330-3).');
        }

        // Gate: 20-day cooling period
        if (lead.dipSentAt) {
            const daysSinceDIP = Math.floor((Date.now() - new Date(lead.dipSentAt).getTime()) / (1000 * 60 * 60 * 24));
            if (daysSinceDIP < COOLING_PERIOD_DAYS) {
                const remaining = COOLING_PERIOD_DAYS - daysSinceDIP;
                throw new BadRequestException(`Délai légal de réflexion non expiré. Il reste ${remaining} jour(s) avant de pouvoir générer le contrat (Art. L330-3 al. 2).`);
            }
        } else {
            throw new BadRequestException('Le DIP n\'a pas été envoyé. Impossible de générer le contrat.');
        }

        const contract = lead.contractDetails ? JSON.parse(lead.contractDetails) : {};
        const typeLabel = contract.type === 'CORNER' ? 'Contrat Corner' : 'Contrat de Franchise';

        const PDFDocument = require('pdfkit');
        const doc = new PDFDocument({ margin: 50 });
        const buffers: Buffer[] = [];
        doc.on('data', buffers.push.bind(buffers));

        return new Promise((resolve) => {
            doc.on('end', async () => {
                const pdfData = Buffer.concat(buffers);

                await this.addNote(id, '📄 Contrat généré (PDF) — Conforme Loi Doubin', 'Système', 'SYSTEM' as any);

                // Auto-advance status
                if (lead.status === 'DIP_SENT') {
                    await this.prisma.franchiseLead.update({
                        where: { id },
                        data: { status: 'CONTRACT_SENT' }
                    });
                    await this.addNote(id, '🚀 Statut mis à jour : CONTRACT_SENT (délai de réflexion respecté)', 'Système', 'SYSTEM' as any);
                }

                resolve(pdfData);
            });

            // === HEADER ===
            doc.fontSize(20).text('CONTRAT DE PARTENARIAT SIMULEGAL', { align: 'center' });
            doc.fontSize(12).text(`Type : ${typeLabel}`, { align: 'center' });
            doc.moveDown(2);

            // === PARTIES ===
            doc.fontSize(14).text('ENTRE LES SOUSSIGNÉS :', { underline: true });
            doc.fontSize(11)
                .text('La société SIMULEGAL HQ, SAS au capital de 10.000€,')
                .text('dont le siège social est situé 8 Rue de la Paix, 75002 Paris,')
                .text('immatriculée au RCS de Paris sous le numéro B 123 456 789,')
                .text('Ci-après dénommée "Le Franchiseur"');
            doc.moveDown();

            doc.fontSize(14).text('ET :', { underline: true });
            if (lead.companyName) {
                doc.fontSize(11)
                    .text(`La société ${lead.companyName}, ${lead.legalForm || 'forme non définie'},`)
                    .text(`immatriculée sous le SIRET ${lead.siret || 'En cours'},`)
                    .text(`Représentée par M./Mme ${lead.name},`)
                    .text(`Domiciliée à ${lead.targetCity} (${lead.region}).`);
            } else {
                doc.fontSize(11).text(`M./Mme ${lead.name}, entrepreneur individuel, domicilié(e) à ${lead.targetCity} (${lead.region}).`);
            }
            doc.text('Ci-après dénommé(e) "Le Partenaire"');
            doc.moveDown(2);

            doc.fontSize(14).text('IL A ÉTÉ PRÉALABLEMENT EXPOSÉ :', { underline: true });
            doc.fontSize(11)
                .text('Le Franchiseur a développé un concept de services juridiques d\'aide à l\'éligibilité sous l\'enseigne SIMULEGAL.')
                .text(`Le Partenaire a reçu le Document d'Information Précontractuelle le ${lead.dipSentAt ? new Date(lead.dipSentAt).toLocaleDateString('fr-FR') : '[date]'}, soit plus de ${COOLING_PERIOD_DAYS} jours avant la signature du présent contrat, conformément à l'article L330-3 du Code de commerce.`);
            doc.moveDown(2);

            doc.fontSize(14).text('IL A ÉTÉ CONVENU CE QUI SUIT :', { underline: true });
            doc.moveDown();

            // === ARTICLE 1 — OBJET ===
            doc.fontSize(12).text('ARTICLE 1 — OBJET', { bold: true });
            doc.fontSize(11).text('Le présent contrat a pour objet de définir les conditions dans lesquelles le Franchiseur concède au Partenaire le droit d\'exploiter le concept SIMULEGAL, incluant l\'usage de la marque, du savoir-faire et de l\'assistance technique.');
            doc.moveDown();

            // === ARTICLE 2 — DURÉE ===
            doc.fontSize(12).text('ARTICLE 2 — DURÉE (Art. R330-1, 4°)', { bold: true });
            const durationText = lead.contractDuration ? `${lead.contractDuration} mois (${(lead.contractDuration / 12).toFixed(1)} ans)` : 'durée indéterminée';
            doc.fontSize(11)
                .text(`Le présent contrat est conclu pour une ${durationText}.`)
                .text(`${lead.renewalTerms || 'Il est renouvelable par tacite reconduction, sauf dénonciation par l\'une des parties dans les conditions prévues à l\'Article 7.'}`);
            doc.moveDown();

            // === ARTICLE 3 — CONDITIONS FINANCIÈRES ===
            doc.fontSize(12).text('ARTICLE 3 — CONDITIONS FINANCIÈRES (Art. R330-1, 3°)', { bold: true });
            const entryFeeEuros = lead.entryFee ? (lead.entryFee / 100).toLocaleString('fr-FR') : '0';
            const royalty = lead.royaltyRate ?? contract.commissionRate ?? 15;
            const adFee = lead.advertisingFee ?? 0;
            doc.fontSize(11)
                .text(`3.1. Droit d'entrée : ${entryFeeEuros} € HT, payable à la signature du contrat.`)
                .text(`3.2. Redevance d'exploitation : ${royalty}% du chiffre d'affaires HT mensuel, payable mensuellement.`)
                .text(`3.3. Contribution au fonds de publicité : ${adFee}% du chiffre d'affaires HT mensuel.`)
                .text('3.4. Les montants ci-dessus s\'entendent hors taxes. La TVA applicable sera facturée en sus.');
            doc.moveDown();

            // === ARTICLE 4 — EXCLUSIVITÉ TERRITORIALE ===
            doc.fontSize(12).text('ARTICLE 4 — ZONE TERRITORIALE (Art. R330-1, 5°)', { bold: true });
            if (lead.exclusiveTerritory) {
                doc.fontSize(11)
                    .text(`Le Partenaire bénéficie d'une exclusivité territoriale dans un rayon de ${lead.exclusiveRadius || 15} km autour de son point de vente situé à ${lead.targetCity}.`)
                    .text('Le Franchiseur s\'engage à ne pas autoriser l\'ouverture d\'un autre point de vente sous l\'enseigne SIMULEGAL dans cette zone pendant la durée du contrat.');
            } else {
                doc.fontSize(11).text('Le présent contrat ne confère au Partenaire aucune exclusivité territoriale.');
            }
            doc.moveDown();

            // === ARTICLE 5 — OBLIGATIONS DU FRANCHISEUR ===
            doc.fontSize(12).text('ARTICLE 5 — OBLIGATIONS DU FRANCHISEUR', { bold: true });
            doc.fontSize(11)
                .text('5.1. Fournir une formation initiale au concept SIMULEGAL.')
                .text('5.2. Mettre à disposition la plateforme logicielle et ses mises à jour.')
                .text('5.3. Assurer une assistance commerciale et technique continue.')
                .text('5.4. Fournir les outils marketing et de communication de l\'enseigne.');
            doc.moveDown();

            // === ARTICLE 6 — OBLIGATIONS DU PARTENAIRE ===
            doc.fontSize(12).text('ARTICLE 6 — OBLIGATIONS DU PARTENAIRE', { bold: true });
            doc.fontSize(11)
                .text('6.1. Respecter les normes et standards de l\'enseigne SIMULEGAL.')
                .text('6.2. S\'acquitter des redevances prévues à l\'Article 3.')
                .text('6.3. Ne pas exercer d\'activité concurrente pendant la durée du contrat.')
                .text('6.4. Transmettre mensuellement les données de chiffre d\'affaires.');
            doc.moveDown();

            // === ARTICLE 7 — RÉSILIATION ===
            doc.fontSize(12).text('ARTICLE 7 — RÉSILIATION (Art. R330-1, 6°)', { bold: true });
            const notice = lead.terminationNotice || 3;
            doc.fontSize(11)
                .text(`7.1. Chaque partie peut résilier le contrat avec un préavis de ${notice} mois par lettre recommandée avec accusé de réception.`)
                .text('7.2. En cas de manquement grave, la résiliation peut être prononcée sans préavis après mise en demeure restée infructueuse pendant 30 jours.')
                .text('7.3. La cession du contrat est soumise à l\'agrément préalable et écrit du Franchiseur.');
            doc.moveDown();

            // === ARTICLE 8 — NON-CONCURRENCE ===
            doc.fontSize(12).text('ARTICLE 8 — CLAUSE DE NON-CONCURRENCE POST-CONTRACTUELLE', { bold: true });
            const nonCompete = lead.nonCompeteDuration;
            if (nonCompete && nonCompete > 0) {
                doc.fontSize(11)
                    .text(`À l'expiration du contrat, le Partenaire s'interdit d'exercer une activité similaire pendant une durée de ${nonCompete} mois, dans un rayon de ${lead.exclusiveRadius || 15} km autour de son ancien point de vente.`);
            } else {
                doc.fontSize(11).text('Le présent contrat ne comporte pas de clause de non-concurrence post-contractuelle.');
            }
            doc.moveDown();

            // === ARTICLE 9 — LOI APPLICABLE ===
            doc.fontSize(12).text('ARTICLE 9 — LOI APPLICABLE ET JURIDICTION', { bold: true });
            doc.fontSize(11)
                .text('Le présent contrat est soumis au droit français.')
                .text('Tout litige sera de la compétence exclusive du Tribunal de Commerce de Paris.');
            doc.moveDown(2);

            // === SIGNATURES ===
            doc.text(`Fait en deux exemplaires à Paris, le ${new Date().toLocaleDateString('fr-FR')}`);
            doc.moveDown(3);
            doc.text('Le Franchiseur                                              Le Partenaire');
            doc.moveDown();
            doc.text('(Signature précédée de la mention "Lu et approuvé")', { italic: true });

            doc.end();
        });
    }

    // ========================================================
    // SIGNATURE — Avec vérification délai 20 jours
    // ========================================================

    async signContract(id: string) {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');
        if (lead.status === 'SIGNED') throw new BadRequestException('Contrat déjà signé.');

        // Gate 1: Le DIP doit avoir été envoyé
        if (!lead.dipSentAt) {
            throw new BadRequestException('Le DIP n\'a pas été envoyé. La signature est impossible sans DIP préalable (Art. L330-3).');
        }

        // Gate 2: Le délai de 20 jours doit être écoulé
        const daysSinceDIP = Math.floor((Date.now() - new Date(lead.dipSentAt).getTime()) / (1000 * 60 * 60 * 24));
        if (daysSinceDIP < COOLING_PERIOD_DAYS) {
            const remaining = COOLING_PERIOD_DAYS - daysSinceDIP;
            throw new BadRequestException(`Délai légal non respecté. Il reste ${remaining} jour(s) de réflexion obligatoire (Art. L330-3 al. 2).`);
        }

        // Gate 3: Le contrat doit avoir été envoyé
        const statusOrder = ['NEW', 'CONTACTED', 'MEETING', 'VALIDATED', 'DIP_SENT', 'CONTRACT_SENT', 'SIGNED'];
        if (statusOrder.indexOf(lead.status) < statusOrder.indexOf('CONTRACT_SENT')) {
            throw new BadRequestException('Le contrat doit être généré et envoyé avant la signature.');
        }

        // 1. Lire les détails du contrat
        const contract = lead.contractDetails ? (typeof lead.contractDetails === 'string' ? JSON.parse(lead.contractDetails) : lead.contractDetails) : {};
        const agencyType = contract.type || 'FRANCHISE';

        // 2. Créer l'agence
        const agencyName = lead.companyName || lead.name;
        const agencyId = `${agencyName.substring(0, 3).toUpperCase()}-${Date.now().toString().slice(-4)}`;

        const agency = await this.agenciesService.create({
            id: agencyId,
            name: agencyName,
            type: agencyType,
            contactEmail: lead.email,
            region: lead.region,
            zipCodes: '[]',
            commissionRate: lead.royaltyRate ?? contract.commissionRate ?? (agencyType === 'CORNER' ? 5 : 15),
            kioskUrl: `https://simulegal.fr/kiosk/${agencyId}`
        });

        // 3. Créer le compte Utilisateur (Gérant)
        const password = generateSecurePassword();
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

        // 6. Envoyer le Kit d'Ouverture (email HTML + WhatsApp)
        await this.notificationsService.onFranchiseOnboarding(lead, password, agency);

        // 7. Log
        await this.addNote(id, `✅ Contrat signé (${daysSinceDIP} jours après envoi du DIP). Agence ${agencyId} créée, compte gérant provisionné, et Kit d'Ouverture envoyé par email + WhatsApp.`, 'Système', 'SYSTEM' as any);

        return {
            lead: updatedLead,
            agency,
            user: { ...user, tempPassword: password }
        };
    }

    // ========================================================
    // KIT D'OUVERTURE PDF — Téléchargeable
    // ========================================================

    async generateOpeningKit(id: string): Promise<Buffer> {
        const lead = await this.prisma.franchiseLead.findUnique({ where: { id } });
        if (!lead) throw new BadRequestException('Lead not found');
        if (lead.status !== 'SIGNED') throw new BadRequestException('Le contrat doit être signé pour générer le kit d\'ouverture.');

        const agency = lead.convertedAgencyId
            ? await this.prisma.agency.findUnique({ where: { id: lead.convertedAgencyId } })
            : null;

        const contract = lead.contractDetails ? JSON.parse(lead.contractDetails) : {};
        const PDFDocument = require('pdfkit');
        const doc = new PDFDocument({ margin: 50 });
        const buffers: Buffer[] = [];
        doc.on('data', buffers.push.bind(buffers));

        return new Promise((resolve) => {
            doc.on('end', async () => {
                resolve(Buffer.concat(buffers));
            });

            // === PAGE 1 : BIENVENUE ===
            doc.rect(0, 0, doc.page.width, 120).fill('#4f46e5');
            doc.fill('#ffffff').fontSize(28).text('KIT D\'OUVERTURE FRANCHISÉ', 50, 35, { align: 'center' });
            doc.fontSize(14).text('SimuLegal — Votre partenaire juridique', { align: 'center' });
            doc.fill('#000000');
            doc.moveDown(3);

            doc.fontSize(16).text(`Bienvenue ${lead.name} !`, { align: 'center' });
            doc.moveDown();
            doc.fontSize(12).text(`Votre agence "${agency?.name || lead.companyName}" (ID: ${agency?.id || '—'}) est maintenant active sur le réseau SimuLegal.`);
            doc.moveDown(2);

            // === SECTION : VOS ACCÈS ===
            doc.fontSize(14).text('🔑 VOS ACCÈS', { underline: true });
            doc.moveDown(0.5);
            doc.fontSize(11)
                .text(`Plateforme : https://admin.simulegal.fr`)
                .text(`Email de connexion : ${lead.email}`)
                .text(`Rôle : Gérant d'agence (AGENCY_MANAGER)`)
                .text(`Agence assignée : ${agency?.id || '—'}`);
            if (agency?.kioskUrl) {
                doc.text(`URL de la borne : ${agency.kioskUrl}`);
            }
            doc.moveDown(2);

            // === SECTION : CHECKLIST D'ONBOARDING ===
            doc.fontSize(14).text('📋 CHECKLIST D\'OUVERTURE', { underline: true });
            doc.moveDown(0.5);
            const steps = [
                { label: 'Se connecter et changer le mot de passe', deadline: 'Immédiat' },
                { label: 'Compléter le profil agence (horaires, adresse, photo)', deadline: 'Jour 1' },
                { label: 'Suivre la formation en ligne (Module Juriste SimuLegal)', deadline: 'Semaine 1' },
                { label: 'Commander les supports marketing (PLV, cartes de visite)', deadline: 'Semaine 1' },
                { label: 'Configurer les services proposés dans votre zone', deadline: 'Semaine 2' },
                { label: 'Effectuer une simulation test (dossier fictif)', deadline: 'Semaine 2' },
                { label: 'Organiser l\'événement d\'inauguration', deadline: 'Mois 1' },
                { label: 'Envoyer le premier reporting mensuel au siège', deadline: 'Mois 1' },
            ];
            steps.forEach((s, i) => {
                doc.fontSize(11).text(`☐ ${i + 1}. ${s.label}`, { continued: true }).text(`  [${s.deadline}]`, { align: 'right' });
            });
            doc.moveDown(2);

            // === SECTION : RÉSUMÉ CONTRAT ===
            doc.fontSize(14).text('📄 RÉSUMÉ DE VOTRE CONTRAT', { underline: true });
            doc.moveDown(0.5);
            const agencyType = contract.type === 'CORNER' ? 'Corner / Borne' : 'Franchise Standard';
            const duration = lead.contractDuration ? `${lead.contractDuration} mois` : 'Indéterminée';
            const royalty = lead.royaltyRate ?? contract.commissionRate ?? 15;
            doc.fontSize(11)
                .text(`Type : ${agencyType}`)
                .text(`Zone : ${lead.targetCity} (${lead.region})`)
                .text(`Durée : ${duration}`)
                .text(`Redevance : ${royalty}%`)
                .text(`Droit d'entrée : ${lead.entryFee ? (lead.entryFee / 100).toLocaleString('fr-FR') + ' €' : 'Aucun'}`)
                .text(`Exclusivité territoriale : ${lead.exclusiveTerritory ? 'Oui (' + (lead.exclusiveRadius || 15) + ' km)' : 'Non'}`)
                .text(`DIP envoyé le : ${lead.dipSentAt ? new Date(lead.dipSentAt).toLocaleDateString('fr-FR') : '—'}`)
                .text(`Contrat signé le : ${new Date(lead.updatedAt).toLocaleDateString('fr-FR')}`);
            doc.moveDown(2);

            // === SECTION : CONTACTS SUPPORT ===
            doc.fontSize(14).text('📞 VOS CONTACTS', { underline: true });
            doc.moveDown(0.5);
            doc.fontSize(11)
                .text('Référent Franchise : [Sera attribué sous 48h]')
                .text('Support Technique : support@simulegal.fr')
                .text('Urgences : 01 23 45 67 89')
                .text('Centre de Formation : formation.simulegal.fr');
            doc.moveDown(2);

            // === MENTIONS LÉGALES ===
            doc.fontSize(8).fill('#94a3b8')
                .text('Ce document est confidentiel. Il est destiné exclusivement au partenaire franchisé désigné.', { align: 'center' })
                .text(`Généré le ${new Date().toLocaleDateString('fr-FR')} — SimuLegal HQ, SAS au capital de 10.000€`, { align: 'center' });

            doc.end();
        });
    }

    // ========================================================
    // VALIDATION SIRET — Via API INSEE
    // ========================================================

    async validateSiret(siret: string): Promise<{ valid: boolean; name?: string; address?: string; error?: string }> {
        // Validation du format (14 chiffres)
        const cleaned = siret.replace(/\s/g, '');
        if (!/^\d{14}$/.test(cleaned)) {
            return { valid: false, error: 'Le SIRET doit contenir exactement 14 chiffres.' };
        }

        // Validation Luhn (algorithme de contrôle SIRET)
        let sum = 0;
        for (let i = 0; i < 14; i++) {
            let digit = parseInt(cleaned[i], 10);
            if (i % 2 === 0) {
                digit *= 2;
                if (digit > 9) digit -= 9;
            }
            sum += digit;
        }
        if (sum % 10 !== 0) {
            return { valid: false, error: 'SIRET invalide (somme de contrôle incorrecte).' };
        }

        // Appel API INSEE (optionnel, si disponible)
        try {
            const response = await fetch(`https://api.insee.fr/entreprises/sirene/V3.11/siret/${cleaned}`, {
                headers: { 'Accept': 'application/json' }
            });
            if (response.ok) {
                const data = await response.json();
                const etab = data?.etablissement;
                return {
                    valid: true,
                    name: etab?.uniteLegale?.denominationUniteLegale || etab?.uniteLegale?.nomUniteLegale || undefined,
                    address: etab?.adresseEtablissement
                        ? `${etab.adresseEtablissement.numeroVoieEtablissement || ''} ${etab.adresseEtablissement.typeVoieEtablissement || ''} ${etab.adresseEtablissement.libelleVoieEtablissement || ''}, ${etab.adresseEtablissement.codePostalEtablissement || ''} ${etab.adresseEtablissement.libelleCommuneEtablissement || ''}`.trim()
                        : undefined
                };
            }
            // API not available (no token) — still valid format
            return { valid: true };
        } catch {
            // API unreachable — format is valid
            return { valid: true };
        }
    }

    // ========================================================
    // COOLING PERIOD STATUS — Pour affichage temps réel
    // ========================================================

    getDIPCoolingStatus(lead: any): { daysElapsed: number; daysRemaining: number; canProceed: boolean; expiresAt: string | null } {
        if (!lead.dipSentAt) {
            return { daysElapsed: 0, daysRemaining: COOLING_PERIOD_DAYS, canProceed: false, expiresAt: null };
        }
        const dipDate = new Date(lead.dipSentAt);
        const daysElapsed = Math.floor((Date.now() - dipDate.getTime()) / (1000 * 60 * 60 * 24));
        const daysRemaining = Math.max(0, COOLING_PERIOD_DAYS - daysElapsed);
        const expiresAt = new Date(dipDate.getTime() + COOLING_PERIOD_DAYS * 24 * 60 * 60 * 1000).toISOString();
        return { daysElapsed, daysRemaining, canProceed: daysRemaining <= 0, expiresAt };
    }

    // ========================================================
    // NOTES & DOCUMENTS
    // ========================================================

    async updateDocuments(id: string, documents: any[]) {
        return this.update(id, { documents: JSON.stringify(documents) });
    }

    async logContractHistory(id: string, version: any) {
        const lead = await this.findOne(id);
        if (!lead) throw new BadRequestException('Lead not found');
        const history = lead.contractHistory || [];
        history.push({
            ...version,
            timestamp: new Date().toISOString()
        });
        return this.update(id, { contractHistory: history });
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

    // ========================================================
    // ANALYTICS & EXPORT
    // ========================================================

    async getAnalytics() {
        const leads = await this.prisma.franchiseLead.findMany({
            select: { status: true, region: true, createdAt: true }
        });

        const statusCounts: Record<string, number> = {};
        leads.forEach(l => { statusCounts[l.status] = (statusCounts[l.status] || 0) + 1; });

        const regionCounts: Record<string, number> = {};
        leads.forEach(l => { regionCounts[l.region] = (regionCounts[l.region] || 0) + 1; });

        const totalNew = leads.length;
        const totalSigned = statusCounts['SIGNED'] || 0;
        const conversionRate = totalNew > 0 ? Math.round((totalSigned / totalNew) * 100) : 0;

        const now = new Date();
        const monthlyTrend: { month: string; count: number; signed: number }[] = [];
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

        return { total: leads.length, statusCounts, regionCounts, conversionRate, monthlyTrend };
    }

    async exportToCSV(filters?: { region?: string; status?: string }): Promise<string> {
        let where: any = {};
        if (filters?.region) where.region = filters.region;
        if (filters?.status) where.status = filters.status;

        const leads = await this.prisma.franchiseLead.findMany({ where, orderBy: { createdAt: 'desc' } });

        const headers = ['ID', 'Nom', 'Email', 'Téléphone', 'Ville', 'Région', 'Statut', 'Société', 'SIRET', 'DIP Envoyé', 'Droit Entrée €', 'Redevance %', 'Date Création'];
        const rows = leads.map(l => [
            l.id, l.name, l.email, l.phone, l.targetCity, l.region, l.status,
            l.companyName || '', l.siret || '',
            l.dipSentAt ? new Date(l.dipSentAt).toLocaleDateString('fr-FR') : '',
            l.entryFee ? (l.entryFee / 100).toFixed(2) : '',
            l.royaltyRate?.toString() || '',
            new Date(l.createdAt).toLocaleDateString('fr-FR')
        ].map(v => `"${(v || '').toString().replace(/"/g, '""')}"`).join(';'));

        return [headers.join(';'), ...rows].join('\n');
    }
}
