import { Injectable, Logger, NotFoundException, BadRequestException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';
import { Cron, CronExpression } from '@nestjs/schedule';
import { createHash } from 'crypto';

@Injectable()
export class BlogService {
    private readonly logger = new Logger(BlogService.name);

    constructor(private prisma: PrismaService) { }

    // ═══════════════════════════════════════════════
    // PUBLIC: Articles
    // ═══════════════════════════════════════════════

    async findPublished(category?: string, limit = 20, offset = 0, search?: string, tag?: string) {
        const where: Record<string, unknown> = { status: 'PUBLISHED' };
        if (category && category !== 'ALL') where.category = category;
        if (tag) where.tags = { contains: tag };
        if (search) {
            where.OR = [
                { title: { contains: search } },
                { excerpt: { contains: search } },
                { tags: { contains: search } },
                { content: { contains: search } },
            ];
            delete where.status; // Search all? No, keep only published.
            where.status = 'PUBLISHED';
        }

        const [articles, total] = await Promise.all([
            this.prisma.article.findMany({
                where,
                orderBy: { publishedAt: 'desc' },
                take: limit,
                skip: offset,
                include: {
                    _count: { select: { comments: { where: { status: 'APPROVED' } }, reactions: true } },
                },
            }),
            this.prisma.article.count({ where }),
        ]);
        return { articles, total, hasMore: offset + limit < total };
    }

    async findBySlug(slug: string, ip?: string) {
        const article = await this.prisma.article.findUnique({
            where: { slug },
            include: {
                comments: {
                    where: { status: 'APPROVED' },
                    orderBy: { createdAt: 'asc' },
                },
                _count: { select: { reactions: true } },
            },
        });
        if (!article || article.status !== 'PUBLISHED') {
            throw new NotFoundException('Article non trouvé');
        }

        // Increment view count + track analytics
        this.prisma.article
            .update({ where: { id: article.id }, data: { viewCount: { increment: 1 } } })
            .catch(() => { });

        if (ip) {
            this.trackAnalytics(article.id, ip).catch(() => { });
        }

        // Get reaction counts by type
        const reactionCounts = await this.prisma.articleReaction.groupBy({
            by: ['type'],
            where: { articleId: article.id },
            _count: true,
        });

        return {
            ...article,
            reactionCounts: reactionCounts.reduce((acc, r) => {
                acc[r.type] = r._count;
                return acc;
            }, {} as Record<string, number>),
        };
    }

    async findFeatured() {
        return this.prisma.article.findMany({
            where: { status: 'PUBLISHED', featured: true },
            orderBy: { publishedAt: 'desc' },
            take: 3,
        });
    }

    // ═══════════════════════════════════════════════
    // REACTIONS (likes)
    // ═══════════════════════════════════════════════

    async addReaction(articleId: string, type: string, ip: string) {
        const validTypes = ['LIKE', 'LOVE', 'FIRE', 'USEFUL'];
        if (!validTypes.includes(type)) throw new BadRequestException('Type de réaction invalide');

        const ipHash = createHash('sha256').update(ip + articleId).digest('hex').substring(0, 32);

        try {
            await this.prisma.articleReaction.create({
                data: { articleId, type, ipHash },
            });
        } catch (e: any) {
            // Unique constraint = already reacted, so remove (toggle)
            if (e?.code === 'P2002') {
                await this.prisma.articleReaction.deleteMany({
                    where: { articleId, ipHash, type },
                });
                return { toggled: 'removed' };
            }
            throw e;
        }
        return { toggled: 'added' };
    }

    async getReactionCounts(articleId: string) {
        const counts = await this.prisma.articleReaction.groupBy({
            by: ['type'],
            where: { articleId },
            _count: true,
        });
        return counts.reduce((acc, r) => {
            acc[r.type] = r._count;
            return acc;
        }, {} as Record<string, number>);
    }

    // ═══════════════════════════════════════════════
    // COMMENTS
    // ═══════════════════════════════════════════════

    async addComment(articleSlug: string, data: {
        authorName: string;
        authorEmail?: string;
        content: string;
        parentId?: string;
        honeypot?: string;
    }) {
        if (data.honeypot) {
            this.logger.warn(`[SPAM] honeypot triggered for ${articleSlug}`);
            return { id: 'fake', success: true };
        }

        const article = await this.prisma.article.findUnique({ where: { slug: articleSlug } });
        if (!article || article.status !== 'PUBLISHED') {
            throw new NotFoundException('Article non trouvé');
        }

        if (data.authorEmail) {
            const oneHourAgo = new Date(Date.now() - 60 * 60 * 1000);
            const recentCount = await this.prisma.articleComment.count({
                where: { authorEmail: data.authorEmail, createdAt: { gte: oneHourAgo } },
            });
            if (recentCount >= 3) {
                throw new BadRequestException('Trop de commentaires. Réessayez plus tard.');
            }
        }

        if (!data.content?.trim() || data.content.length < 3) throw new BadRequestException('Le commentaire est trop court.');
        if (data.content.length > 2000) throw new BadRequestException('Le commentaire est trop long (2000 caractères max).');

        const comment = await this.prisma.articleComment.create({
            data: {
                articleId: article.id,
                authorName: data.authorName.trim().substring(0, 100),
                authorEmail: data.authorEmail?.trim()?.substring(0, 200),
                content: data.content.trim(),
                parentId: data.parentId || null,
                status: 'PENDING',
            },
        });

        this.logger.log(`New comment on "${article.title}" by ${data.authorName}`);
        return comment;
    }

    async getComments(articleId: string, includeAll = false) {
        return this.prisma.articleComment.findMany({
            where: { articleId, ...(includeAll ? {} : { status: 'APPROVED' }) },
            orderBy: { createdAt: 'desc' },
        });
    }

    async moderateComment(commentId: string, status: 'APPROVED' | 'REJECTED') {
        return this.prisma.articleComment.update({ where: { id: commentId }, data: { status } });
    }

    async getPendingComments() {
        return this.prisma.articleComment.findMany({
            where: { status: 'PENDING' },
            orderBy: { createdAt: 'desc' },
            include: { article: { select: { title: true, slug: true } } },
        });
    }

    async deleteComment(commentId: string) {
        await this.prisma.articleComment.delete({ where: { id: commentId } });
        return { success: true };
    }

    // ═══════════════════════════════════════════════
    // NEWSLETTER
    // ═══════════════════════════════════════════════

    async subscribe(email: string, name?: string) {
        const existing = await this.prisma.newsletterSubscriber.findUnique({ where: { email } });
        if (existing) {
            if (existing.status === 'UNSUBSCRIBED') {
                await this.prisma.newsletterSubscriber.update({
                    where: { email },
                    data: { status: 'ACTIVE', unsubscribedAt: null, confirmed: true },
                });
                return { success: true, message: 'Réabonnement effectué' };
            }
            return { success: true, message: 'Déjà inscrit' };
        }

        const confirmToken = createHash('sha256').update(email + Date.now()).digest('hex').substring(0, 32);
        await this.prisma.newsletterSubscriber.create({
            data: { email, name, confirmToken, confirmed: true }, // auto-confirm for simplicity
        });
        this.logger.log(`Newsletter subscription: ${email}`);
        return { success: true, message: 'Inscription réussie !' };
    }

    async unsubscribe(token: string) {
        const sub = await this.prisma.newsletterSubscriber.findUnique({ where: { confirmToken: token } });
        if (!sub) throw new NotFoundException('Lien invalide');
        await this.prisma.newsletterSubscriber.update({
            where: { id: sub.id },
            data: { status: 'UNSUBSCRIBED', unsubscribedAt: new Date() },
        });
        return { success: true };
    }

    async getSubscribers() {
        return this.prisma.newsletterSubscriber.findMany({
            where: { status: 'ACTIVE', confirmed: true },
            orderBy: { subscribedAt: 'desc' },
        });
    }

    async getSubscriberStats() {
        const [active, total] = await Promise.all([
            this.prisma.newsletterSubscriber.count({ where: { status: 'ACTIVE' } }),
            this.prisma.newsletterSubscriber.count(),
        ]);
        return { active, total, unsubscribed: total - active };
    }

    // ═══════════════════════════════════════════════
    // ANALYTICS
    // ═══════════════════════════════════════════════

    private async trackAnalytics(articleId: string, ip: string) {
        const today = new Date();
        today.setHours(0, 0, 0, 0);

        try {
            await this.prisma.articleAnalytics.upsert({
                where: { articleId_date: { articleId, date: today } },
                create: { articleId, date: today, views: 1, uniqueIps: 1 },
                update: { views: { increment: 1 } },
            });
        } catch {
            // Silently fail — analytics are best-effort
        }
    }

    async getArticleAnalytics(articleId: string, days = 30) {
        const since = new Date();
        since.setDate(since.getDate() - days);

        return this.prisma.articleAnalytics.findMany({
            where: { articleId, date: { gte: since } },
            orderBy: { date: 'asc' },
        });
    }

    async getDashboardAnalytics(days = 30) {
        const since = new Date();
        since.setDate(since.getDate() - days);

        const [totalArticles, publishedCount, totalViews, featuredCount, totalComments, totalReactions, recentAnalytics, topArticles] = await Promise.all([
            this.prisma.article.count(),
            this.prisma.article.count({ where: { status: 'PUBLISHED' } }),
            this.prisma.article.aggregate({ _sum: { viewCount: true } }),
            this.prisma.article.count({ where: { featured: true } }),
            this.prisma.articleComment.count({ where: { status: 'APPROVED' } }),
            this.prisma.articleReaction.count(),
            this.prisma.articleAnalytics.findMany({
                where: { date: { gte: since } },
                orderBy: { date: 'asc' },
            }),
            this.prisma.article.findMany({
                where: { status: 'PUBLISHED' },
                orderBy: { viewCount: 'desc' },
                take: 5,
                select: { id: true, title: true, slug: true, viewCount: true, publishedAt: true },
            }),
        ]);

        // Aggregate daily views
        const dailyViews: Record<string, number> = {};
        for (const a of recentAnalytics) {
            const day = new Date(a.date).toISOString().split('T')[0];
            dailyViews[day] = (dailyViews[day] || 0) + a.views;
        }

        return {
            totalArticles,
            publishedCount,
            totalViews: totalViews._sum.viewCount || 0,
            featuredCount,
            totalComments,
            totalReactions,
            topArticles,
            dailyViews,
        };
    }

    // ═══════════════════════════════════════════════
    // REVISIONS (Version History)
    // ═══════════════════════════════════════════════

    private async createRevision(articleId: string, title: string, content: string, excerpt?: string, editedBy?: string) {
        const lastRevision = await this.prisma.articleRevision.findFirst({
            where: { articleId },
            orderBy: { version: 'desc' },
        });
        return this.prisma.articleRevision.create({
            data: {
                articleId,
                title,
                content,
                excerpt,
                editedBy,
                version: (lastRevision?.version || 0) + 1,
            },
        });
    }

    async getRevisions(articleId: string) {
        return this.prisma.articleRevision.findMany({
            where: { articleId },
            orderBy: { version: 'desc' },
            select: { id: true, version: true, title: true, editedBy: true, createdAt: true },
        });
    }

    async getRevision(revisionId: string) {
        return this.prisma.articleRevision.findUnique({ where: { id: revisionId } });
    }

    async restoreRevision(articleId: string, revisionId: string) {
        const revision = await this.prisma.articleRevision.findUnique({ where: { id: revisionId } });
        if (!revision || revision.articleId !== articleId) throw new NotFoundException('Révision non trouvée');

        // Save current state as a revision before restoring
        const current = await this.prisma.article.findUnique({ where: { id: articleId } });
        if (current) {
            await this.createRevision(articleId, current.title, current.content, current.excerpt || undefined, 'Avant restauration');
        }

        return this.prisma.article.update({
            where: { id: articleId },
            data: { title: revision.title, content: revision.content, excerpt: revision.excerpt },
        });
    }

    // ═══════════════════════════════════════════════
    // BLOG CATEGORIES (dynamic)
    // ═══════════════════════════════════════════════

    async getCategories() {
        return this.prisma.blogCategory.findMany({ orderBy: { sortOrder: 'asc' } });
    }

    async createCategory(data: { name: string; icon?: string; color?: string }) {
        const slug = this.generateSlug(data.name);
        return this.prisma.blogCategory.create({
            data: { name: data.name, slug, icon: data.icon, color: data.color },
        });
    }

    async updateCategory(id: string, data: Partial<{ name: string; icon: string; color: string; sortOrder: number }>) {
        return this.prisma.blogCategory.update({ where: { id }, data });
    }

    async deleteCategory(id: string) {
        await this.prisma.blogCategory.delete({ where: { id } });
        return { success: true };
    }

    // ═══════════════════════════════════════════════
    // RSS FEED
    // ═══════════════════════════════════════════════

    async generateRssFeed(baseUrl: string): Promise<string> {
        const articles = await this.prisma.article.findMany({
            where: { status: 'PUBLISHED' },
            orderBy: { publishedAt: 'desc' },
            take: 30,
        });

        const escXml = (s: string) => s
            .replace(/&/g, '&amp;').replace(/</g, '&lt;')
            .replace(/>/g, '&gt;').replace(/"/g, '&quot;');

        const items = articles.map(a => `
    <item>
      <title>${escXml(a.title)}</title>
      <link>${baseUrl}/blog/${escXml(a.slug)}</link>
      <guid isPermaLink="true">${baseUrl}/blog/${escXml(a.slug)}</guid>
      <description>${escXml(a.excerpt || '')}</description>
      <pubDate>${a.publishedAt ? new Date(a.publishedAt).toUTCString() : ''}</pubDate>
      <category>${escXml(a.category)}</category>
      <dc:creator>${escXml(a.authorName)}</dc:creator>
    </item>`).join('');

        return `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>SimuLegal — Blog &amp; Insights</title>
    <link>${baseUrl}/blog</link>
    <description>Actualités et guides juridiques — immigration, naturalisation, titre de séjour</description>
    <language>fr</language>
    <lastBuildDate>${new Date().toUTCString()}</lastBuildDate>
    <atom:link href="${baseUrl}/api/blog/rss" rel="self" type="application/rss+xml" />${items}
  </channel>
</rss>`;
    }

    // ═══════════════════════════════════════════════
    // SITEMAP
    // ═══════════════════════════════════════════════

    async generateSitemap(baseUrl: string): Promise<string> {
        const articles = await this.prisma.article.findMany({
            where: { status: 'PUBLISHED' },
            select: { slug: true, updatedAt: true, publishedAt: true },
            orderBy: { publishedAt: 'desc' },
        });

        const escXml = (s: string) => s.replace(/&/g, '&amp;');

        const urls = articles.map(a => `
  <url>
    <loc>${escXml(baseUrl)}/blog/${escXml(a.slug)}</loc>
    <lastmod>${(a.updatedAt || a.publishedAt || new Date()).toISOString()}</lastmod>
    <changefreq>monthly</changefreq>
    <priority>0.7</priority>
  </url>`).join('');

        return `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  <url>
    <loc>${escXml(baseUrl)}</loc>
    <changefreq>weekly</changefreq>
    <priority>1.0</priority>
  </url>
  <url>
    <loc>${escXml(baseUrl)}/blog</loc>
    <changefreq>daily</changefreq>
    <priority>0.9</priority>
  </url>${urls}
</urlset>`;
    }

    // ═══════════════════════════════════════════════
    // SCHEDULED PUBLICATIONS (CRON)
    // ═══════════════════════════════════════════════

    @Cron(CronExpression.EVERY_MINUTE)
    async publishScheduledArticles() {
        const now = new Date();
        const scheduled = await this.prisma.article.findMany({
            where: { status: 'SCHEDULED', scheduledAt: { lte: now } },
        });

        for (const article of scheduled) {
            await this.prisma.article.update({
                where: { id: article.id },
                data: { status: 'PUBLISHED', publishedAt: now },
            });
            this.logger.log(`📅 Auto-published scheduled article: "${article.title}"`);
        }
    }

    // ═══════════════════════════════════════════════
    // ADMIN CRUD
    // ═══════════════════════════════════════════════

    async findAll(status?: string) {
        const where: Record<string, unknown> = {};
        if (status && status !== 'ALL') where.status = status;
        return this.prisma.article.findMany({
            where,
            orderBy: { updatedAt: 'desc' },
            include: {
                _count: { select: { comments: true, reactions: true } },
            },
        });
    }

    async findOne(id: string) {
        const article = await this.prisma.article.findUnique({
            where: { id },
            include: { comments: { orderBy: { createdAt: 'desc' } } },
        });
        if (!article) throw new NotFoundException('Article non trouvé');
        return article;
    }

    async create(data: {
        title: string; slug?: string; excerpt?: string; content: string;
        category?: string; status?: string; coverImage?: string;
        authorName?: string; authorRole?: string; tags?: string;
        metaTitle?: string; metaDescription?: string;
        readTimeMin?: number; featured?: boolean; scheduledAt?: string;
    }) {
        const slug = data.slug || this.generateSlug(data.title);
        const existing = await this.prisma.article.findUnique({ where: { slug } });
        const uniqueSlug = existing ? `${slug}-${Date.now()}` : slug;
        const readTimeMin = data.readTimeMin || Math.max(1, Math.ceil((data.content || '').split(/\s+/).length / 200));

        let status = data.status || 'DRAFT';
        let publishedAt: Date | null = null;
        let scheduledAt: Date | null = null;

        if (status === 'PUBLISHED') {
            publishedAt = new Date();
        } else if (status === 'SCHEDULED' && data.scheduledAt) {
            scheduledAt = new Date(data.scheduledAt);
        }

        const article = await this.prisma.article.create({
            data: {
                title: data.title, slug: uniqueSlug,
                excerpt: data.excerpt || data.content.substring(0, 160) + '...',
                content: data.content, category: data.category || 'GENERAL',
                status, coverImage: data.coverImage,
                authorName: data.authorName || 'Rédaction SimuLegal',
                authorRole: data.authorRole, tags: data.tags,
                metaTitle: data.metaTitle, metaDescription: data.metaDescription,
                readTimeMin, featured: data.featured || false,
                publishedAt, scheduledAt,
            },
        });

        // Create initial revision
        await this.createRevision(article.id, article.title, article.content, article.excerpt || undefined, article.authorName);

        this.logger.log(`Article created: ${article.title} (${article.id})`);
        return article;
    }

    async update(id: string, data: Record<string, unknown>) {
        const existing = await this.findOne(id);
        const updateData: Record<string, unknown> = { ...data };

        if (data.status === 'PUBLISHED' && !existing.publishedAt) {
            updateData.publishedAt = new Date();
        }
        if (data.status === 'SCHEDULED' && data.scheduledAt) {
            updateData.scheduledAt = new Date(data.scheduledAt as string);
        }

        // Create revision if content or title changed
        if (data.content || data.title) {
            await this.createRevision(
                id,
                (data.title as string) || existing.title,
                (data.content as string) || existing.content,
                (data.excerpt as string) || existing.excerpt || undefined,
                (data.authorName as string) || existing.authorName,
            );
        }

        return this.prisma.article.update({ where: { id }, data: updateData });
    }

    async remove(id: string) {
        await this.findOne(id);
        await this.prisma.article.delete({ where: { id } });
        this.logger.log(`Article deleted: ${id}`);
        return { success: true };
    }

    // ═══════════════════════════════════════════════
    // EXPORT / IMPORT
    // ═══════════════════════════════════════════════

    async exportArticles(format: 'json' | 'markdown' = 'json') {
        const articles = await this.prisma.article.findMany({
            orderBy: { createdAt: 'desc' },
            include: { comments: true },
        });

        if (format === 'markdown') {
            return articles.map(a => ({
                filename: `${a.slug}.md`,
                content: `---\ntitle: "${a.title}"\nslug: "${a.slug}"\ncategory: "${a.category}"\nstatus: "${a.status}"\nauthor: "${a.authorName}"\ntags: "${a.tags || ''}"\ndate: "${a.publishedAt?.toISOString() || a.createdAt.toISOString()}"\n---\n\n${a.content}`,
            }));
        }
        return articles;
    }

    async importArticle(data: { title: string; content: string; slug?: string; category?: string; status?: string; tags?: string }) {
        return this.create({ ...data, status: data.status || 'DRAFT' });
    }

    // ═══════════════════════════════════════════════
    // UTILS
    // ═══════════════════════════════════════════════

    private generateSlug(title: string): string {
        return title
            .toLowerCase()
            .normalize('NFD')
            .replace(/[\u0300-\u036f]/g, '')
            .replace(/[^a-z0-9]+/g, '-')
            .replace(/^-|-$/g, '')
            .substring(0, 80);
    }
}
