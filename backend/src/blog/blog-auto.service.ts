import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { BlogService } from './blog.service';
import { Cron, CronExpression } from '@nestjs/schedule';

/**
 * BlogAutoService — Pipeline de veille légale → génération automatique d'articles
 * 
 * Workflow:
 * 1. DISCOVER: Scrape les sources légales (RSS, Legifrance, service-public.fr)
 * 2. SCORE: Évalue la pertinence de chaque sujet pour l'audience SimuLegal
 * 3. GENERATE: Utilise l'IA pour rédiger un article de blog complet
 * 4. REVIEW: L'article est créé en statut DRAFT/REVIEW pour validation admin
 * 5. PUBLISH: L'admin valide et publie
 */
@Injectable()
export class BlogAutoService {
    private readonly logger = new Logger(BlogAutoService.name);

    // ── Predefined legal sources ──
    private readonly LEGAL_SOURCES = [
        {
            name: 'Legifrance',
            rssUrl: 'https://www.legifrance.gouv.fr/rss/loda.xml',
            categories: ['IMMIGRATION', 'NATURALISATION', 'SEJOUR', 'FAMILY'],
        },
        {
            name: 'service-public.fr',
            rssUrl: 'https://www.service-public.fr/P10001/rss/list',
            categories: ['GENERAL', 'SEJOUR', 'PERMIS'],
        },
        {
            name: 'Vie Publique',
            rssUrl: 'https://www.vie-publique.fr/rss/actualites.xml',
            categories: ['GENERAL', 'IMMIGRATION'],
        },
    ];

    // ── Relevance keywords by category ──
    private readonly RELEVANCE_KEYWORDS: Record<string, string[]> = {
        IMMIGRATION: ['immigration', 'étranger', 'visa', 'asile', 'réfugié', 'titre de séjour', 'OQTF', 'régularisation', 'droit des étrangers', 'CESEDA'],
        NATURALISATION: ['naturalisation', 'nationalité', 'française', 'décret', 'acquisition', 'intégration', 'citoyenneté'],
        SEJOUR: ['titre de séjour', 'carte de séjour', 'récépissé', 'autorisation de travail', 'VPF', 'vie privée', 'préfecture', 'ANEF'],
        PERMIS: ['permis de conduire', 'échange de permis', 'conversion', 'permis étranger', 'code de la route'],
        FAMILY: ['regroupement familial', 'rapprochement familial', 'conjoint', 'mariage', 'mineur', 'OFII', 'allocations'],
    };

    constructor(
        private prisma: PrismaService,
        private blogService: BlogService,
    ) { }

    // ═══════════════════════════════════════════════
    // CRON: Scheduled topic discovery
    // ═══════════════════════════════════════════════

    @Cron(CronExpression.EVERY_DAY_AT_6AM)
    async scheduledDiscovery() {
        const config = await this.getOrCreateConfig();
        if (!config.enabled) return;

        // Check frequency
        if (config.lastRunAt) {
            const daysSinceLastRun = (Date.now() - new Date(config.lastRunAt).getTime()) / (1000 * 60 * 60 * 24);
            const freqDays: Record<string, number> = { DAILY: 1, WEEKLY: 7, BIWEEKLY: 14, MONTHLY: 30 };
            if (daysSinceLastRun < (freqDays[config.frequency] || 7)) {
                this.logger.debug('[BlogAuto] Skipping — not time yet');
                return;
            }
        }

        this.logger.log('📡 [BlogAuto] Starting scheduled topic discovery...');
        await this.discoverTopics();
        await this.generateDrafts();

        await this.prisma.blogAutoConfig.update({
            where: { id: config.id },
            data: { lastRunAt: new Date() },
        });
    }

    // ═══════════════════════════════════════════════
    // STEP 1: Discover Topics
    // ═══════════════════════════════════════════════

    async discoverTopics(): Promise<{ discovered: number; sources: string[] }> {
        this.logger.log('🔍 [BlogAuto] Discovering topics from legal sources...');
        const config = await this.getOrCreateConfig();
        const targetCats = config.targetCategories.split(',');
        let totalDiscovered = 0;
        const usedSources: string[] = [];

        // 1. Import from existing LegalUpdates (veille module)
        const recentUpdates = await this.prisma.legalUpdate.findMany({
            where: { createdAt: { gte: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000) } },
            orderBy: { createdAt: 'desc' },
            take: 20,
        });

        for (const update of recentUpdates) {
            // Check if already exists as topic
            const existing = await this.prisma.blogAutoTopic.findFirst({
                where: { legalUpdateId: update.id },
            });
            if (existing) continue;

            const score = this.calculateRelevanceScore(update.title, update.summary, update.category);
            if (score < config.minRelevanceScore) continue;

            await this.prisma.blogAutoTopic.create({
                data: {
                    title: update.title,
                    summary: update.summary,
                    sourceUrl: update.sourceUrl || null,
                    sourceName: 'Veille Juridique Interne',
                    category: this.mapCategory(update.category, targetCats),
                    relevanceScore: score,
                    keywords: this.extractKeywords(update.title + ' ' + update.summary),
                    legalUpdateId: update.id,
                    status: 'DISCOVERED',
                },
            });
            totalDiscovered++;
        }
        if (recentUpdates.length > 0) usedSources.push('Veille Juridique Interne');

        // 2. Try RSS feeds (wrapped in try/catch for resilience)
        for (const source of this.LEGAL_SOURCES) {
            try {
                const rssTopics = await this.fetchRssTopics(source.rssUrl, source.name, targetCats, config.minRelevanceScore);
                totalDiscovered += rssTopics;
                if (rssTopics > 0) usedSources.push(source.name);
            } catch (err) {
                this.logger.warn(`[BlogAuto] RSS fetch failed for ${source.name}: ${(err as Error).message}`);
            }
        }

        // 3. Generate synthetic topics based on trending legal themes
        const syntheticCount = await this.generateSyntheticTopics(targetCats, config.minRelevanceScore);
        if (syntheticCount > 0) {
            totalDiscovered += syntheticCount;
            usedSources.push('Analyse Thématique');
        }

        this.logger.log(`📡 [BlogAuto] Discovered ${totalDiscovered} new topics from ${usedSources.join(', ')}`);
        return { discovered: totalDiscovered, sources: usedSources };
    }

    private async fetchRssTopics(rssUrl: string, sourceName: string, targetCats: string[], minScore: number): Promise<number> {
        try {
            const res = await fetch(rssUrl, { signal: AbortSignal.timeout(10000) });
            if (!res.ok) return 0;

            const xml = await res.text();
            const items = this.parseRssItems(xml);
            let count = 0;

            for (const item of items.slice(0, 10)) {
                // Dedup by title similarity
                const existing = await this.prisma.blogAutoTopic.findFirst({
                    where: { title: { contains: item.title.substring(0, 30) } },
                });
                if (existing) continue;

                const score = this.calculateRelevanceScore(item.title, item.description, '');
                if (score < minScore) continue;

                const category = this.detectCategory(item.title + ' ' + item.description, targetCats);

                await this.prisma.blogAutoTopic.create({
                    data: {
                        title: item.title,
                        summary: item.description || item.title,
                        sourceUrl: item.link,
                        sourceName,
                        category,
                        relevanceScore: score,
                        keywords: this.extractKeywords(item.title + ' ' + (item.description || '')),
                        status: 'DISCOVERED',
                    },
                });
                count++;
            }
            return count;
        } catch {
            return 0;
        }
    }

    private async generateSyntheticTopics(targetCats: string[], minScore: number): Promise<number> {
        // Generate topics based on recent trends and common queries
        const syntheticTopics = [
            {
                title: 'Les changements récents en matière de titres de séjour ' + new Date().getFullYear(),
                summary: 'Tour d\'horizon des dernières modifications réglementaires affectant les demandes de titres de séjour en France.',
                category: 'SEJOUR',
            },
            {
                title: 'Naturalisation : les critères d\'évaluation du niveau de français',
                summary: 'Analyse détaillée des exigences linguistiques pour la naturalisation française et comment s\'y préparer.',
                category: 'NATURALISATION',
            },
            {
                title: 'Les délais de traitement des demandes d\'immigration en préfecture',
                summary: 'État des lieux des temps d\'attente par type de demande et par préfecture.',
                category: 'IMMIGRATION',
            },
            {
                title: 'Regroupement familial : guide des conditions de ressources ' + new Date().getFullYear(),
                summary: 'Comprendre les seuils de revenus exigés et les justificatifs à fournir pour un regroupement familial.',
                category: 'FAMILY',
            },
            {
                title: 'Échange de permis de conduire étranger : la procédure ANTS',
                summary: 'Guide pas à pas pour convertir un permis de conduire étranger via la plateforme ANTS.',
                category: 'PERMIS',
            },
        ];

        let count = 0;
        for (const topic of syntheticTopics) {
            if (!targetCats.includes(topic.category)) continue;

            // Check if similar topic already exists
            const existing = await this.prisma.blogAutoTopic.findFirst({
                where: { title: { contains: topic.title.substring(0, 30) } },
            });
            if (existing) continue;

            const score = this.calculateRelevanceScore(topic.title, topic.summary, topic.category);
            if (score < minScore) continue;

            await this.prisma.blogAutoTopic.create({
                data: {
                    ...topic,
                    sourceName: 'Analyse Thématique',
                    relevanceScore: score,
                    keywords: this.extractKeywords(topic.title + ' ' + topic.summary),
                    status: 'DISCOVERED',
                },
            });
            count++;
        }
        return count;
    }

    // ═══════════════════════════════════════════════
    // STEP 2: Score & Rank Topics
    // ═══════════════════════════════════════════════

    private calculateRelevanceScore(title: string, summary: string, category: string): number {
        const text = (title + ' ' + summary + ' ' + category).toLowerCase();
        let score = 30; // Base score

        // Check relevance keywords
        for (const [cat, keywords] of Object.entries(this.RELEVANCE_KEYWORDS)) {
            for (const kw of keywords) {
                if (text.includes(kw.toLowerCase())) {
                    score += 10;
                    break; // Max 10 per category
                }
            }
        }

        // Bonus for legal specificity
        const legalTerms = ['décret', 'loi', 'circulaire', 'arrêté', 'directive', 'règlement', 'ordonnance', 'code', 'article L', 'article R'];
        for (const term of legalTerms) {
            if (text.includes(term.toLowerCase())) {
                score += 5;
            }
        }

        // Bonus for current year references
        const currentYear = new Date().getFullYear().toString();
        if (text.includes(currentYear)) score += 10;

        // Penalty for too generic
        if (text.length < 50) score -= 15;

        return Math.min(100, Math.max(0, score));
    }

    private detectCategory(text: string, targetCats: string[]): string {
        const lower = text.toLowerCase();
        let bestCat = 'GENERAL';
        let bestScore = 0;

        for (const [cat, keywords] of Object.entries(this.RELEVANCE_KEYWORDS)) {
            if (!targetCats.includes(cat)) continue;
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

    private mapCategory(originalCat: string, targetCats: string[]): string {
        const upper = originalCat.toUpperCase();
        if (targetCats.includes(upper)) return upper;
        return 'GENERAL';
    }

    // ═══════════════════════════════════════════════
    // STEP 3: Generate Article Drafts (AI)
    // ═══════════════════════════════════════════════

    async generateDrafts(): Promise<{ generated: number; errors: number }> {
        const config = await this.getOrCreateConfig();

        // Get top un-generated topics
        const topics = await this.prisma.blogAutoTopic.findMany({
            where: { status: 'DISCOVERED', relevanceScore: { gte: config.minRelevanceScore } },
            orderBy: { relevanceScore: 'desc' },
            take: config.maxArticlesPerRun,
        });

        let generated = 0;
        let errors = 0;

        for (const topic of topics) {
            try {
                await this.prisma.blogAutoTopic.update({
                    where: { id: topic.id },
                    data: { status: 'GENERATING' },
                });

                // Generate article content using AI
                const articleContent = await this.generateArticleWithAI(topic, config);

                // Create article as DRAFT (for admin review)
                const article = await this.blogService.create({
                    title: articleContent.title,
                    content: articleContent.content,
                    excerpt: articleContent.excerpt,
                    category: topic.category,
                    status: 'DRAFT', // Admin must review!
                    tags: topic.keywords || '',
                    authorName: '🤖 SimuLegal AI',
                    authorRole: 'Rédaction automatisée',
                    metaTitle: articleContent.metaTitle,
                    metaDescription: articleContent.metaDescription,
                    readTimeMin: Math.ceil(articleContent.content.split(/\s+/).length / 200),
                });

                await this.prisma.blogAutoTopic.update({
                    where: { id: topic.id },
                    data: { status: 'GENERATED', generatedArticleId: article.id },
                });

                await this.prisma.blogAutoConfig.update({
                    where: { id: config.id },
                    data: { totalGenerated: { increment: 1 } },
                });

                this.logger.log(`🤖 [BlogAuto] Generated article: "${article.title}" from topic "${topic.title}"`);
                generated++;
            } catch (err) {
                errors++;
                await this.prisma.blogAutoTopic.update({
                    where: { id: topic.id },
                    data: { status: 'DISCOVERED', error: (err as Error).message },
                });
                this.logger.error(`[BlogAuto] Generation failed for "${topic.title}": ${(err as Error).message}`);
            }
        }

        return { generated, errors };
    }

    private async generateArticleWithAI(
        topic: { title: string; summary: string; category: string; keywords?: string | null; sourceUrl?: string | null },
        config: { aiModel: string; aiPromptTemplate?: string | null },
    ): Promise<{ title: string; content: string; excerpt: string; metaTitle: string; metaDescription: string }> {
        const openaiKey = process.env.OPENAI_API_KEY;

        if (!openaiKey) {
            // Fallback: generate a structured template article without AI
            return this.generateTemplateArticle(topic);
        }

        const prompt = config.aiPromptTemplate || this.getDefaultPrompt();
        const systemPrompt = prompt
            .replace('{TITLE}', topic.title)
            .replace('{SUMMARY}', topic.summary)
            .replace('{CATEGORY}', topic.category)
            .replace('{KEYWORDS}', topic.keywords || '')
            .replace('{SOURCE_URL}', topic.sourceUrl || '');

        try {
            const res = await fetch('https://api.openai.com/v1/chat/completions', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${openaiKey}`,
                },
                body: JSON.stringify({
                    model: config.aiModel || 'gpt-4o-mini',
                    messages: [
                        { role: 'system', content: 'Tu es un juriste expert en droit des étrangers en France, rédacteur pour le blog SimuLegal.' },
                        { role: 'user', content: systemPrompt },
                    ],
                    temperature: 0.7,
                    max_tokens: 3000,
                    response_format: { type: 'json_object' },
                }),
                signal: AbortSignal.timeout(60000),
            });

            if (!res.ok) {
                throw new Error(`OpenAI API error: ${res.status}`);
            }

            const data = await res.json();
            const content = data.choices?.[0]?.message?.content;
            if (!content) throw new Error('Empty AI response');

            const parsed = JSON.parse(content);
            return {
                title: parsed.title || topic.title,
                content: parsed.content || '',
                excerpt: parsed.excerpt || topic.summary,
                metaTitle: parsed.metaTitle || parsed.title || topic.title,
                metaDescription: parsed.metaDescription || topic.summary.substring(0, 160),
            };
        } catch (err) {
            this.logger.warn(`[BlogAuto] AI generation failed, using template: ${(err as Error).message}`);
            return this.generateTemplateArticle(topic);
        }
    }

    private getDefaultPrompt(): string {
        return `Rédige un article de blog juridique complet en français sur le sujet suivant.

Sujet : {TITLE}
Contexte : {SUMMARY}
Catégorie : {CATEGORY}
Mots-clés : {KEYWORDS}
Source : {SOURCE_URL}

L'article doit :
- Être rédigé en Markdown
- Faire entre 800 et 1500 mots
- Avoir un ton professionnel mais accessible
- Inclure des sous-titres (## et ###)
- Citer les textes de loi pertinents
- Donner des conseils pratiques
- Terminer par une conclusion avec appel à action vers SimuLegal

Réponds en JSON avec les champs :
{
  "title": "Titre de l'article",
  "content": "Contenu en Markdown",
  "excerpt": "Résumé en 160 caractères",
  "metaTitle": "Titre SEO",
  "metaDescription": "Description SEO en 160 caractères"
}`;
    }

    private generateTemplateArticle(
        topic: { title: string; summary: string; category: string; keywords?: string | null; sourceUrl?: string | null },
    ): { title: string; content: string; excerpt: string; metaTitle: string; metaDescription: string } {
        const year = new Date().getFullYear();
        const source = topic.sourceUrl ? `\n\n> 📎 **Source** : [${topic.sourceUrl}](${topic.sourceUrl})` : '';

        const content = `## ${topic.title}

${topic.summary}

### Ce que vous devez savoir

Cette actualité juridique concerne directement les personnes engagées dans des démarches de ${topic.category.toLowerCase()} en France. Voici les points essentiels à retenir.${source}

### Les implications pratiques

Les changements réglementaires dans ce domaine peuvent avoir des conséquences significatives sur vos démarches en cours ou à venir. Il est important de vous tenir informé des évolutions légales.

### Nos recommandations

1. **Vérifiez votre situation** : Utilisez notre simulateur d'éligibilité pour évaluer l'impact sur votre dossier
2. **Constituez votre dossier** : Rassemblez les pièces justificatives nécessaires
3. **Anticipez les délais** : Les modifications réglementaires peuvent entraîner des changements dans les temps de traitement

### Besoin d'accompagnement ?

SimuLegal vous aide à naviguer ces changements. Notre simulateur intelligent analyse votre profil et vous indique les démarches les plus adaptées à votre situation.

---

*Article rédigé par l'assistant IA SimuLegal — ${year}. Cet article est fourni à titre informatif et ne constitue pas un conseil juridique.*`;

        return {
            title: topic.title,
            content,
            excerpt: topic.summary.substring(0, 160),
            metaTitle: `${topic.title} — SimuLegal ${year}`,
            metaDescription: topic.summary.substring(0, 155) + '...',
        };
    }

    // ═══════════════════════════════════════════════
    // RSS Parsing (simple XML)
    // ═══════════════════════════════════════════════

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

    private extractKeywords(text: string): string {
        const allKeywords = Object.values(this.RELEVANCE_KEYWORDS).flat();
        const lower = text.toLowerCase();
        const found = allKeywords.filter(kw => lower.includes(kw.toLowerCase()));
        return [...new Set(found)].slice(0, 5).join(',');
    }

    // ═══════════════════════════════════════════════
    // ADMIN API
    // ═══════════════════════════════════════════════

    async getTopics(status?: string) {
        const where: Record<string, unknown> = {};
        if (status) where.status = status;
        return this.prisma.blogAutoTopic.findMany({
            where,
            orderBy: { createdAt: 'desc' },
        });
    }

    async getTopicById(id: string) {
        return this.prisma.blogAutoTopic.findUnique({ where: { id } });
    }

    async rejectTopic(id: string) {
        return this.prisma.blogAutoTopic.update({
            where: { id },
            data: { status: 'REJECTED' },
        });
    }

    async retryTopic(id: string) {
        return this.prisma.blogAutoTopic.update({
            where: { id },
            data: { status: 'DISCOVERED', error: null },
        });
    }

    async getOrCreateConfig() {
        let config = await this.prisma.blogAutoConfig.findFirst();
        if (!config) {
            config = await this.prisma.blogAutoConfig.create({ data: {} });
        }
        return config;
    }

    async updateConfig(data: Partial<{
        enabled: boolean;
        frequency: string;
        maxArticlesPerRun: number;
        minRelevanceScore: number;
        targetCategories: string;
        aiModel: string;
        aiPromptTemplate: string;
    }>) {
        const config = await this.getOrCreateConfig();
        return this.prisma.blogAutoConfig.update({
            where: { id: config.id },
            data,
        });
    }

    async getStats() {
        const [total, discovered, generating, generated, rejected, published] = await Promise.all([
            this.prisma.blogAutoTopic.count(),
            this.prisma.blogAutoTopic.count({ where: { status: 'DISCOVERED' } }),
            this.prisma.blogAutoTopic.count({ where: { status: 'GENERATING' } }),
            this.prisma.blogAutoTopic.count({ where: { status: 'GENERATED' } }),
            this.prisma.blogAutoTopic.count({ where: { status: 'REJECTED' } }),
            this.prisma.blogAutoTopic.count({ where: { status: 'PUBLISHED' } }),
        ]);
        const config = await this.getOrCreateConfig();
        return {
            total, discovered, generating, generated, rejected, published,
            config: {
                enabled: config.enabled,
                frequency: config.frequency,
                lastRunAt: config.lastRunAt,
                totalGenerated: config.totalGenerated,
            },
        };
    }

    // Manual trigger for admin
    async triggerDiscoveryAndGeneration(): Promise<{ discovered: number; generated: number; errors: number }> {
        const disc = await this.discoverTopics();
        const gen = await this.generateDrafts();
        return { discovered: disc.discovered, generated: gen.generated, errors: gen.errors };
    }
}
