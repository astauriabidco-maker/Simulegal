import { Injectable, Logger, NotFoundException, BadRequestException } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class BlogService {
    private readonly logger = new Logger(BlogService.name);

    constructor(private prisma: PrismaService) { }

    // ── Public: list published articles ──
    async findPublished(category?: string, limit = 20, offset = 0) {
        const where: Record<string, unknown> = { status: 'PUBLISHED' };
        if (category && category !== 'ALL') {
            where.category = category;
        }
        const [articles, total] = await Promise.all([
            this.prisma.article.findMany({
                where,
                orderBy: { publishedAt: 'desc' },
                take: limit,
                skip: offset,
                include: { _count: { select: { comments: { where: { status: 'APPROVED' } } } } },
            }),
            this.prisma.article.count({ where }),
        ]);
        return { articles, total };
    }

    // ── Public: get article by slug ──
    async findBySlug(slug: string) {
        const article = await this.prisma.article.findUnique({
            where: { slug },
            include: {
                comments: {
                    where: { status: 'APPROVED' },
                    orderBy: { createdAt: 'desc' },
                },
            },
        });
        if (!article || article.status !== 'PUBLISHED') {
            throw new NotFoundException('Article non trouvé');
        }
        // Increment view count (fire and forget)
        this.prisma.article
            .update({ where: { id: article.id }, data: { viewCount: { increment: 1 } } })
            .catch(() => { });
        return article;
    }

    // ── Public: featured articles ──
    async findFeatured() {
        return this.prisma.article.findMany({
            where: { status: 'PUBLISHED', featured: true },
            orderBy: { publishedAt: 'desc' },
            take: 3,
        });
    }

    // ═══════════════════════════════════════════════
    // COMMENTS
    // ═══════════════════════════════════════════════

    async addComment(articleSlug: string, data: {
        authorName: string;
        authorEmail?: string;
        content: string;
        parentId?: string;
        honeypot?: string; // anti-spam
    }) {
        // Anti-spam: honeypot
        if (data.honeypot) {
            this.logger.warn(`[SPAM] honeypot triggered for ${articleSlug}`);
            return { id: 'fake', success: true }; // silent fail
        }

        const article = await this.prisma.article.findUnique({ where: { slug: articleSlug } });
        if (!article || article.status !== 'PUBLISHED') {
            throw new NotFoundException('Article non trouvé');
        }

        // Rate limit: max 3 comments/hour per email
        if (data.authorEmail) {
            const oneHourAgo = new Date(Date.now() - 60 * 60 * 1000);
            const recentCount = await this.prisma.articleComment.count({
                where: { authorEmail: data.authorEmail, createdAt: { gte: oneHourAgo } },
            });
            if (recentCount >= 3) {
                throw new BadRequestException('Trop de commentaires. Réessayez plus tard.');
            }
        }

        // Validate content
        if (!data.content?.trim() || data.content.length < 3) {
            throw new BadRequestException('Le commentaire est trop court.');
        }
        if (data.content.length > 2000) {
            throw new BadRequestException('Le commentaire est trop long (2000 caractères max).');
        }

        const comment = await this.prisma.articleComment.create({
            data: {
                articleId: article.id,
                authorName: data.authorName.trim().substring(0, 100),
                authorEmail: data.authorEmail?.trim()?.substring(0, 200),
                content: data.content.trim(),
                parentId: data.parentId || null,
                status: 'PENDING', // needs moderation
            },
        });

        this.logger.log(`New comment on "${article.title}" by ${data.authorName}`);
        return comment;
    }

    async getComments(articleId: string, includeAll = false) {
        return this.prisma.articleComment.findMany({
            where: {
                articleId,
                ...(includeAll ? {} : { status: 'APPROVED' }),
            },
            orderBy: { createdAt: 'desc' },
        });
    }

    async moderateComment(commentId: string, status: 'APPROVED' | 'REJECTED') {
        return this.prisma.articleComment.update({
            where: { id: commentId },
            data: { status },
        });
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
    // ADMIN
    // ═══════════════════════════════════════════════

    async findAll(status?: string) {
        const where: Record<string, unknown> = {};
        if (status && status !== 'ALL') {
            where.status = status;
        }
        return this.prisma.article.findMany({
            where,
            orderBy: { updatedAt: 'desc' },
            include: { _count: { select: { comments: true } } },
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
        title: string;
        slug?: string;
        excerpt?: string;
        content: string;
        category?: string;
        status?: string;
        coverImage?: string;
        authorName?: string;
        authorRole?: string;
        tags?: string;
        metaTitle?: string;
        metaDescription?: string;
        readTimeMin?: number;
        featured?: boolean;
    }) {
        const slug = data.slug || this.generateSlug(data.title);
        const existing = await this.prisma.article.findUnique({ where: { slug } });
        const uniqueSlug = existing ? `${slug}-${Date.now()}` : slug;
        const publishedAt = data.status === 'PUBLISHED' ? new Date() : null;
        const readTimeMin = data.readTimeMin || Math.max(1, Math.ceil((data.content || '').split(/\s+/).length / 200));

        const article = await this.prisma.article.create({
            data: {
                title: data.title,
                slug: uniqueSlug,
                excerpt: data.excerpt || data.content.substring(0, 160) + '...',
                content: data.content,
                category: data.category || 'GENERAL',
                status: data.status || 'DRAFT',
                coverImage: data.coverImage,
                authorName: data.authorName || 'Rédaction SimuLegal',
                authorRole: data.authorRole,
                tags: data.tags,
                metaTitle: data.metaTitle,
                metaDescription: data.metaDescription,
                readTimeMin,
                featured: data.featured || false,
                publishedAt,
            },
        });

        this.logger.log(`Article created: ${article.title} (${article.id})`);
        return article;
    }

    async update(id: string, data: Partial<{
        title: string;
        slug: string;
        excerpt: string;
        content: string;
        category: string;
        status: string;
        coverImage: string;
        authorName: string;
        authorRole: string;
        tags: string;
        metaTitle: string;
        metaDescription: string;
        readTimeMin: number;
        featured: boolean;
    }>) {
        await this.findOne(id);
        const updateData: Record<string, unknown> = { ...data };
        if (data.status === 'PUBLISHED') {
            const existing = await this.prisma.article.findUnique({ where: { id } });
            if (existing && !existing.publishedAt) {
                updateData.publishedAt = new Date();
            }
        }
        return this.prisma.article.update({ where: { id }, data: updateData });
    }

    async remove(id: string) {
        await this.findOne(id);
        await this.prisma.article.delete({ where: { id } });
        this.logger.log(`Article deleted: ${id}`);
        return { success: true };
    }

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
