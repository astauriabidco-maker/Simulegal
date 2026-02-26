import { Injectable, Logger, OnModuleInit } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Cron, CronExpression } from '@nestjs/schedule';

/**
 * VeilleService — Veille Juridique Automatisée
 *
 * Workflow:
 * 1. DISCOVER: Scrape les flux RSS des sources légales (Legifrance, service-public.fr, Vie Publique)
 * 2. SCORE: Évalue la pertinence de chaque item par mots-clés juridiques
 * 3. DEDUP: Ignore les items déjà présents (par titre ou URL source)
 * 4. CREATE: Crée automatiquement des LegalUpdate en statut `applied: false`
 * 5. NOTIFY: Log le nombre de nouvelles notes détectées
 *
 * Les juristes/admins peuvent ensuite :
 * - Consulter la source originale
 * - Marquer la note comme "Appliquée" (impact intégré dans les règles)
 * - Lier la note aux règles d'éligibilité impactées
 */
@Injectable()
export class VeilleService implements OnModuleInit {
    private readonly logger = new Logger(VeilleService.name);

    // ─── Sources RSS juridiques ─────────────────────────────────
    private readonly LEGAL_SOURCES = [
        {
            name: 'Legifrance',
            rssUrl: 'https://www.legifrance.gouv.fr/rss/loda.xml',
            categories: ['Immigration Professionnelle', 'Naturalisation', 'Regroupement Familial', 'Réglementation Générale'],
        },
        {
            name: 'service-public.fr',
            rssUrl: 'https://www.service-public.fr/P10001/rss/list',
            categories: ['Réglementation Générale', 'Immigration Professionnelle'],
        },
        {
            name: 'Vie Publique',
            rssUrl: 'https://www.vie-publique.fr/rss/actualites.xml',
            categories: ['Réglementation Générale', 'Immigration Professionnelle'],
        },
    ];

    // ─── Mots-clés de scoring par catégorie ─────────────────────
    private readonly RELEVANCE_KEYWORDS: Record<string, string[]> = {
        'Immigration Professionnelle': [
            'immigration', 'étranger', 'visa', 'titre de séjour', 'passeport talent',
            'autorisation de travail', 'OQTF', 'régularisation', 'CESEDA', 'ANEF',
            'carte de résident', 'récépissé', 'droit des étrangers', 'séjour',
        ],
        'Naturalisation': [
            'naturalisation', 'nationalité', 'française', 'acquisition', 'intégration',
            'citoyenneté', 'décret de naturalisation', 'assimilation',
        ],
        'Regroupement Familial': [
            'regroupement familial', 'rapprochement familial', 'conjoint', 'OFII',
            'vie privée et familiale', 'VPF', 'mineur étranger', 'réunification',
        ],
        'Droit d\'asile': [
            'asile', 'réfugié', 'protection subsidiaire', 'OFPRA', 'CNDA',
            'demandeur d\'asile', 'pays sûr', 'non-refoulement',
        ],
        'Réglementation Générale': [
            'décret', 'loi', 'circulaire', 'arrêté', 'préfecture', 'ordonnance',
            'code de l\'entrée', 'directive européenne',
        ],
    };

    constructor(private prisma: PrismaService) { }

    async onModuleInit() {
        await this.seedDefaultNotes();
    }

    // ═══════════════════════════════════════════════════════════
    // CRON: Veille automatisée — tous les jours à 7h
    // ═══════════════════════════════════════════════════════════

    @Cron(CronExpression.EVERY_DAY_AT_7AM)
    async scheduledScan() {
        this.logger.log('📡 [Veille] Lancement du scan quotidien...');
        const result = await this.scanSources();
        if (result.created > 0) {
            this.logger.log(`📡 [Veille] ✅ ${result.created} nouvelle(s) note(s) détectée(s) depuis ${result.sourcesUsed.join(', ')}`);
        } else {
            this.logger.debug('[Veille] Aucune nouvelle note détectée aujourd\'hui.');
        }
    }

    // ═══════════════════════════════════════════════════════════
    // SCAN: Parcours des sources RSS et création de LegalUpdate
    // ═══════════════════════════════════════════════════════════

    async scanSources(): Promise<{ created: number; errors: number; sourcesUsed: string[] }> {
        let created = 0;
        let errors = 0;
        const sourcesUsed: string[] = [];

        for (const source of this.LEGAL_SOURCES) {
            try {
                const count = await this.fetchAndCreateFromRss(source);
                if (count > 0) {
                    created += count;
                    sourcesUsed.push(source.name);
                }
            } catch (err) {
                errors++;
                this.logger.warn(`[Veille] Erreur RSS ${source.name}: ${(err as Error).message}`);
            }
        }

        return { created, errors, sourcesUsed };
    }

    private async fetchAndCreateFromRss(source: { name: string; rssUrl: string; categories: string[] }): Promise<number> {
        const res = await fetch(source.rssUrl, { signal: AbortSignal.timeout(15000) });
        if (!res.ok) {
            this.logger.warn(`[Veille] RSS ${source.name} returned ${res.status}`);
            return 0;
        }

        const xml = await res.text();
        const items = this.parseRssItems(xml);
        let count = 0;

        for (const item of items.slice(0, 15)) {
            // Scoring: l'item doit être pertinent pour nos domaines
            const score = this.calculateRelevanceScore(item.title, item.description);
            if (score < 30) continue; // Seuil minimal

            // Déduplication par titre OU par sourceUrl
            const existing = await this.prisma.legalUpdate.findFirst({
                where: {
                    OR: [
                        { title: item.title },
                        ...(item.link ? [{ sourceUrl: item.link }] : []),
                    ],
                },
            });
            if (existing) continue;

            // Déterminer la catégorie et la sévérité
            const category = this.detectCategory(item.title + ' ' + item.description, source.categories);
            const severity = this.detectSeverity(item.title + ' ' + item.description, score);

            await this.prisma.legalUpdate.create({
                data: {
                    title: item.title,
                    summary: item.description || `Évolution juridique détectée depuis ${source.name}. Consultez la source pour plus de détails.`,
                    category,
                    severity,
                    sourceUrl: item.link || null,
                    authorName: `🤖 Veille Auto (${source.name})`,
                    applied: false,
                    linkedRuleIds: '[]',
                },
            });
            count++;
        }

        return count;
    }

    // ═══════════════════════════════════════════════════════════
    // SCORING & CLASSIFICATION
    // ═══════════════════════════════════════════════════════════

    private calculateRelevanceScore(title: string, description: string): number {
        const text = (title + ' ' + (description || '')).toLowerCase();
        let score = 10; // Base

        // Mots-clés par catégorie (+8 par hit, max 1 par catégorie)
        for (const keywords of Object.values(this.RELEVANCE_KEYWORDS)) {
            for (const kw of keywords) {
                if (text.includes(kw.toLowerCase())) {
                    score += 8;
                    break;
                }
            }
        }

        // Bonus termes juridiques spécifiques
        const legalTerms = ['décret', 'loi n°', 'circulaire', 'arrêté', 'directive', 'ordonnance', 'article L', 'article R', 'JO du', 'journal officiel'];
        for (const term of legalTerms) {
            if (text.includes(term.toLowerCase())) score += 5;
        }

        // Bonus année en cours
        if (text.includes(new Date().getFullYear().toString())) score += 8;

        // Pénalité contenu trop court
        if (text.length < 40) score -= 15;

        return Math.min(100, Math.max(0, score));
    }

    private detectCategory(text: string, availableCats: string[]): string {
        const lower = text.toLowerCase();
        let bestCat = availableCats[availableCats.length - 1] || 'Réglementation Générale';
        let bestScore = 0;

        for (const [cat, keywords] of Object.entries(this.RELEVANCE_KEYWORDS)) {
            if (!availableCats.includes(cat)) continue;
            let catScore = 0;
            for (const kw of keywords) {
                if (lower.includes(kw.toLowerCase())) catScore++;
            }
            if (catScore > bestScore) {
                bestScore = catScore;
                bestCat = cat;
            }
        }
        return bestCat;
    }

    private detectSeverity(text: string, score: number): string {
        const lower = text.toLowerCase();

        // High: textes fondamentaux
        const highTerms = ['décret', 'loi n°', 'abrogation', 'réforme', 'suppression', 'obligation', 'entrée en vigueur'];
        if (highTerms.some(t => lower.includes(t)) || score >= 70) return 'high';

        // Medium: circulaires, changements de procédure
        const mediumTerms = ['circulaire', 'modification', 'précision', 'instruction', 'mise à jour', 'arrêté'];
        if (mediumTerms.some(t => lower.includes(t)) || score >= 45) return 'medium';

        return 'low';
    }

    // ═══════════════════════════════════════════════════════════
    // RSS PARSING (XML simple sans dépendance)
    // ═══════════════════════════════════════════════════════════

    private parseRssItems(xml: string): { title: string; link: string; description: string }[] {
        const items: { title: string; link: string; description: string }[] = [];
        const itemRegex = /<item>([\s\S]*?)<\/item>/g;
        let match;
        while ((match = itemRegex.exec(xml)) !== null) {
            const itemXml = match[1];
            const title = this.extractXmlTag(itemXml, 'title');
            const link = this.extractXmlTag(itemXml, 'link');
            const description = this.extractXmlTag(itemXml, 'description');
            if (title) items.push({ title, link: link || '', description: description || '' });
        }
        return items;
    }

    private extractXmlTag(xml: string, tag: string): string {
        const regex = new RegExp(`<${tag}[^>]*>(?:<!\\[CDATA\\[)?([\\s\\S]*?)(?:\\]\\]>)?<\\/${tag}>`, 'i');
        const match = regex.exec(xml);
        return match ? match[1].trim() : '';
    }

    // ═══════════════════════════════════════════════════════════
    // CRUD EXISTANT
    // ═══════════════════════════════════════════════════════════

    async findAll() {
        return this.prisma.legalUpdate.findMany({
            orderBy: { createdAt: 'desc' },
        });
    }

    async findPending() {
        return this.prisma.legalUpdate.findMany({
            where: { applied: false },
            orderBy: { createdAt: 'desc' },
        });
    }

    async create(data: {
        title: string;
        summary: string;
        category: string;
        severity?: string;
        sourceUrl?: string;
        authorName?: string;
        linkedRuleIds?: string[];
    }) {
        const note = await this.prisma.legalUpdate.create({
            data: {
                title: data.title,
                summary: data.summary,
                category: data.category,
                severity: data.severity || 'medium',
                sourceUrl: data.sourceUrl || null,
                authorName: data.authorName || null,
                linkedRuleIds: JSON.stringify(data.linkedRuleIds || []),
            },
        });
        this.logger.log(`✅ Note créée: "${note.title}" (${note.id})`);
        return note;
    }

    async update(id: string, data: Partial<{
        title: string;
        summary: string;
        category: string;
        severity: string;
        sourceUrl: string;
        authorName: string;
        applied: boolean;
        linkedRuleIds: string[];
    }>) {
        const { linkedRuleIds, ...rest } = data;
        const updateData: any = { ...rest };
        if (linkedRuleIds !== undefined) {
            updateData.linkedRuleIds = JSON.stringify(linkedRuleIds);
        }
        const note = await this.prisma.legalUpdate.update({
            where: { id },
            data: updateData,
        });
        this.logger.log(`✅ Note mise à jour: "${note.title}" (${note.id})`);
        return note;
    }

    async markAsApplied(id: string) {
        return this.prisma.legalUpdate.update({
            where: { id },
            data: { applied: true, appliedAt: new Date() },
        });
    }

    async remove(id: string) {
        await this.prisma.legalUpdate.delete({ where: { id } });
        this.logger.log(`🗑 Note supprimée: ${id}`);
        return { deleted: true };
    }

    async getStats() {
        const [total, applied, pending, autoCount] = await Promise.all([
            this.prisma.legalUpdate.count(),
            this.prisma.legalUpdate.count({ where: { applied: true } }),
            this.prisma.legalUpdate.count({ where: { applied: false } }),
            this.prisma.legalUpdate.count({ where: { authorName: { startsWith: '🤖' } } }),
        ]);

        const bySeverity = await Promise.all([
            this.prisma.legalUpdate.count({ where: { severity: 'high', applied: false } }),
            this.prisma.legalUpdate.count({ where: { severity: 'medium', applied: false } }),
            this.prisma.legalUpdate.count({ where: { severity: 'low', applied: false } }),
        ]);

        const latest = await this.prisma.legalUpdate.findFirst({
            orderBy: { createdAt: 'desc' },
            select: { createdAt: true },
        });

        return {
            totalCount: total,
            appliedCount: applied,
            pendingCount: pending,
            autoDetectedCount: autoCount,
            pendingByPriority: { high: bySeverity[0], medium: bySeverity[1], low: bySeverity[2] },
            conformityPercent: total > 0 ? Math.round((applied / total) * 100) : 100,
            lastUpdate: latest?.createdAt || null,
        };
    }

    // ═══════════════════════════════════════════════════════════
    // SEED — Notes par défaut si DB vide
    // ═══════════════════════════════════════════════════════════

    private async seedDefaultNotes() {
        const count = await this.prisma.legalUpdate.count();
        if (count > 0) return;

        const seeds = [
            {
                title: 'Décret n°2026-XXX modifiant les conditions du Passeport Talent',
                summary: 'Modification des seuils de rémunération minimale pour le passeport talent. Le seuil salarié passe de 1.5x à 1.8x le SMIC pour les professions non-tendues.',
                category: 'Immigration Professionnelle',
                severity: 'high',
                sourceUrl: 'https://www.legifrance.gouv.fr',
                authorName: '🤖 Veille Auto (Legifrance)',
            },
            {
                title: 'Circulaire sur l\'accélération des demandes de naturalisation',
                summary: 'Nouvelle instruction visant à réduire les délais de traitement des demandes de naturalisation à 12 mois maximum.',
                category: 'Naturalisation',
                severity: 'medium',
                sourceUrl: 'https://www.service-public.fr',
                authorName: '🤖 Veille Auto (service-public.fr)',
            },
            {
                title: 'Décision du Conseil d\'État sur la preuve de cohabitation VPF',
                summary: 'Nouvelle décision du Conseil d\'État précisant les critères de preuve de cohabitation continue pour les demandes de titre VPF.',
                category: 'Regroupement Familial',
                severity: 'low',
                sourceUrl: '',
                authorName: 'Me. Dupont',
                applied: true,
                appliedAt: new Date('2026-01-12'),
            },
        ];

        for (const seed of seeds) {
            await this.prisma.legalUpdate.create({
                data: {
                    ...seed,
                    linkedRuleIds: '[]',
                    applied: seed.applied || false,
                    appliedAt: seed.appliedAt || null,
                },
            });
        }

        this.logger.log(`[Veille] ✅ ${seeds.length} notes de veille seedées`);
    }
}
