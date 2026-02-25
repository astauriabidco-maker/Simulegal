import { Injectable, Logger } from '@nestjs/common';
import { OnEvent } from '@nestjs/event-emitter';
import { Cron, CronExpression } from '@nestjs/schedule';
import { PrismaService } from '../prisma/prisma.service';
import { NotificationsService } from '../notifications/notifications.service';
import { OllamaTextService } from './ollama-text.service';

@Injectable()
export class SupervisionAgentService {
    private readonly logger = new Logger(SupervisionAgentService.name);

    constructor(
        private prisma: PrismaService,
        private notifications: NotificationsService,
        private ollamaText: OllamaTextService
    ) { }

    /**
     * Agent réveillé par la réception d'un message textuel WhatsApp
     */
    @OnEvent('whatsapp.message.received', { async: true })
    async handleIncomingWhatsApp(payload: { leadId?: string, prospectId?: string, message: string, senderName: string, senderPhone: string }) {
        this.logger.log(`🤖 [Supervision Agent] Analyse sémantique demandée pour le message de ${payload.senderName}`);

        const analysis = await this.ollamaText.analyzeCustomerMessage(payload.message);
        if (!analysis) return;

        this.logger.log(`🤖 [Supervision Agent] Intention: ${analysis.intent} | Urgence: ${analysis.urngecy}`);

        // Si le message nécessite une attention (urgent ou actionable)
        if (analysis.actionable || analysis.urngecy === 'HIGH') {
            const warningMessage = `🤖 **Résumé Agent IA** :\n${analysis.summary}\n\n**Intention** : ${analysis.intent}\n**Urgence** : ${analysis.urngecy}\n\n💡 *Conseil* : ${analysis.reasoning}`;
            const authorSignature = '🤖 Assistant IA';

            if (payload.leadId) {
                await this.prisma.leadNote.create({
                    data: {
                        content: warningMessage,
                        author: authorSignature,
                        leadId: payload.leadId,
                    }
                });
            } else if (payload.prospectId) {
                await this.prisma.prospectNote.create({
                    data: {
                        text: warningMessage,
                        authorId: authorSignature,
                        prospectId: payload.prospectId,
                    }
                });
            }
        }
    }

    /**
     * Agent réveillé par l'événement de validation d'un nouveau document
     * @param payload 
     */
    @OnEvent('lead.document.validated', { async: true })
    async handleDocumentValidated(payload: { leadId: string }) {
        this.logger.log(`🤖 [Supervision Agent] Réveil pour vérifier le dossier ${payload.leadId}`);
        await this.checkDossierConsistency(payload.leadId);
    }

    private async checkDossierConsistency(leadId: string) {
        // Récupérer le lead et ses documents
        const lead = await this.prisma.lead.findUnique({
            where: { id: leadId }
        });

        if (!lead || !lead.documents) return;

        const documents: any[] = JSON.parse(lead.documents) || [];
        const validDocs = documents.filter(d => d.status === 'VALID' && d.ocrData);

        if (validDocs.length < 2) {
            // Pas assez de documents pour faire une vérification croisée
            return;
        }

        const inconsistencies: string[] = [];

        // --- Cross-Check: Vérification des Noms ---
        const namesFound = new Set<string>();
        validDocs.forEach(doc => {
            if (doc.ocrData.lastName) {
                // Normaliser: Majuscules et trim
                namesFound.add(doc.ocrData.lastName.trim().toUpperCase());
            }
        });

        if (namesFound.size > 1) {
            inconsistencies.push(
                `- Conflit sur le Noms de Famille détecté entre les documents : ${Array.from(namesFound).join(' vs ')}`
            );
        }

        // --- Cross-Check: Vérification des Dates de Naissance ---
        const birthDatesFound = new Set<string>();
        validDocs.forEach(doc => {
            if (doc.ocrData.birthDate) {
                birthDatesFound.add(doc.ocrData.birthDate.trim());
            }
        });

        if (birthDatesFound.size > 1) {
            inconsistencies.push(
                `- Conflit sur la Date de Naissance détecté entre les documents : ${Array.from(birthDatesFound).join(' vs ')}`
            );
        }

        // S'il y a des incohérences, alerter !
        if (inconsistencies.length > 0) {
            const warningMessage = `🚨 **Alerte de l'Agent de Supervision**\n\nDes incohérences ont été détectées dans les données extraites des différents documents envoyés par ce prospect :\n${inconsistencies.join('\n')}\n\n*Merci de vérifier manuellement les dossiers juridiques complets.*`;

            this.logger.warn(`🤖 [Supervision Agent] Incohérence trouvée sur ${leadId} : ${inconsistencies.join(' | ')}`);

            // 1. Ajouter une note dans le CRM (Lead Note) pour le juriste
            await this.prisma.leadNote.create({
                data: {
                    content: warningMessage,
                    author: '🤖 Agent Supervision QA',
                    leadId: leadId,
                }
            });

            // 2. (Optionnel) Logger un évènement de communication interne
            await this.prisma.communication.create({
                data: {
                    direction: 'INBOUND',
                    type: 'SYSTEM_ALERT',
                    content: warningMessage,
                    sender: 'SYSTEM',
                    senderName: '🤖 Agent QA',
                    leadId: leadId
                }
            });
        } else {
            this.logger.log(`🤖 [Supervision Agent] Dossier ${leadId} fluide et consistant (0 incohérence textuelle)`);
        }
    }

    /**
     * Tâche de fond CRON (déclenchée chaque minute en DEV, ou EVERY_DAY_AT_MIDNIGHT en PROD)
     * Scanne les dossiers bloqués (impayés, en attente de documents depuis trop longtemps etc.)
     */
    @Cron(CronExpression.EVERY_DAY_AT_MIDNIGHT)
    async checkNightlyStuckAndUnpaidLeads() {
        this.logger.log(`🤖 [Supervision Agent] Début du scan nocturne des dossiers lents et impayés...`);

        const sevenDaysAgo = new Date();
        sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);

        const twoDaysAgo = new Date();
        twoDaysAgo.setDate(twoDaysAgo.getDate() - 2);

        // 1. Détection des impayés (Leads ou Prospects convertis sans paiement après 48h)
        const unpaidLeads = await this.prisma.lead.findMany({
            where: {
                amountPaid: 0,
                createdAt: { lte: twoDaysAgo }
            }
        });

        for (const lead of unpaidLeads) {
            // Check si on a déjà fait la note d'alerte impayé
            const existingAlert = await this.prisma.leadNote.findFirst({
                where: { leadId: lead.id, content: { contains: 'Urgence: HIGH' } }
            });

            if (!existingAlert) {
                const warningMessage = `🤖 **Résumé Agent IA** :\nAttention, ce dossier a été créé il y a plus de 48h (le ${lead.createdAt.toLocaleDateString()}) et aucun paiement (0€) n'a encore été encaissé sur le système.\n\n**Intention** : Suivi Paiement\n**Urgence** : HIGH\n\n💡 *Conseil* : Relancer immédiatement ou suspendre la prestation en envoyant un Magic Link de paiement WhatsApp.`;

                await this.prisma.leadNote.create({
                    data: {
                        content: warningMessage,
                        author: '🤖 Assistant IA',
                        leadId: lead.id,
                    }
                });
                this.logger.warn(`🤖 [Supervision Agent] Impayé détecté et alerté sur le lead ${lead.id}`);
            }
        }

        // 2. Détection des dossiers inactifs/bloqués sur COLLECTING depuis + 7 jours
        const stuckLeads = await this.prisma.lead.findMany({
            where: {
                status: 'COLLECTING',
                stageEnteredAt: { lte: sevenDaysAgo }
            }
        });

        for (const lead of stuckLeads) {
            const existingAlert = await this.prisma.leadNote.findFirst({
                where: { leadId: lead.id, content: { contains: 'Pièces Manquantes' } }
            });

            if (!existingAlert) {
                const requiredDocs = lead.requiredDocs ? JSON.parse(lead.requiredDocs) : [];
                const currentDocs = lead.documents ? JSON.parse(lead.documents) : [];

                const missingOrRejectedCount = requiredDocs.length - currentDocs.filter((d: any) => d.status === 'VALID').length;

                const warningMessage = `🤖 **Résumé Agent IA** :\nLe client semble bloqué. Il reste bloqué à l'étape Collecte depuis plus de 7 jours. Il manque encore ${missingOrRejectedCount} document(s) valide(s).\n\n**Intention** : Pièces Manquantes\n**Urgence** : MEDIUM\n\n💡 *Conseil* : Envoyer un message WhatsApp de courtoisie pour proposer de l'aide sur le rassemblement des pièces administratives.`;

                await this.prisma.leadNote.create({
                    data: {
                        content: warningMessage,
                        author: '🤖 Assistant IA',
                        leadId: lead.id,
                    }
                });
                this.logger.warn(`🤖 [Supervision Agent] Dossier lent détecté pour collecte sur le lead ${lead.id}`);
            }
        }

        this.logger.log(`🤖 [Supervision Agent] Scan nocturne terminé.`);
    }
}
