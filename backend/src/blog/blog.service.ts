import { Injectable, Logger, NotFoundException } from '@nestjs/common';
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
            }),
            this.prisma.article.count({ where }),
        ]);
        return { articles, total };
    }

    // ── Public: get article by slug ──
    async findBySlug(slug: string) {
        const article = await this.prisma.article.findUnique({ where: { slug } });
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

    // ── Admin: list all articles ──
    async findAll(status?: string) {
        const where: Record<string, unknown> = {};
        if (status && status !== 'ALL') {
            where.status = status;
        }
        return this.prisma.article.findMany({
            where,
            orderBy: { updatedAt: 'desc' },
        });
    }

    // ── Admin: get single article by id ──
    async findOne(id: string) {
        const article = await this.prisma.article.findUnique({ where: { id } });
        if (!article) throw new NotFoundException('Article non trouvé');
        return article;
    }

    // ── Admin: create article ──
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

        // Ensure slug is unique
        const existing = await this.prisma.article.findUnique({ where: { slug } });
        const uniqueSlug = existing ? `${slug}-${Date.now()}` : slug;

        const publishedAt = data.status === 'PUBLISHED' ? new Date() : null;

        // Estimate read time if not provided
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

    // ── Admin: update article ──
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
        await this.findOne(id); // throws if not found

        // If publishing for the first time, set publishedAt
        const updateData: Record<string, unknown> = { ...data };
        if (data.status === 'PUBLISHED') {
            const existing = await this.prisma.article.findUnique({ where: { id } });
            if (existing && !existing.publishedAt) {
                updateData.publishedAt = new Date();
            }
        }

        return this.prisma.article.update({
            where: { id },
            data: updateData,
        });
    }

    // ── Admin: delete article ──
    async remove(id: string) {
        await this.findOne(id);
        await this.prisma.article.delete({ where: { id } });
        this.logger.log(`Article deleted: ${id}`);
        return { success: true };
    }

    // ── Utility ──
    private generateSlug(title: string): string {
        return title
            .toLowerCase()
            .normalize('NFD')
            .replace(/[\u0300-\u036f]/g, '') // remove accents
            .replace(/[^a-z0-9]+/g, '-')
            .replace(/^-|-$/g, '')
            .substring(0, 80);
    }
}
