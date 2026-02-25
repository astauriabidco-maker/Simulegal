import { Test, TestingModule } from '@nestjs/testing';
import { BlogService } from './blog.service';
import { PrismaService } from '../prisma/prisma.service';
import { NotFoundException, BadRequestException } from '@nestjs/common';

// ── Prisma mock factory ──
const mockPrisma = () => ({
    article: {
        findMany: jest.fn(),
        findUnique: jest.fn(),
        create: jest.fn(),
        update: jest.fn(),
        delete: jest.fn(),
        count: jest.fn(),
    },
    articleComment: {
        findMany: jest.fn(),
        create: jest.fn(),
        update: jest.fn(),
        delete: jest.fn(),
        count: jest.fn(),
    },
});

describe('BlogService', () => {
    let service: BlogService;
    let prisma: ReturnType<typeof mockPrisma>;

    beforeEach(async () => {
        prisma = mockPrisma();
        const module: TestingModule = await Test.createTestingModule({
            providers: [
                BlogService,
                { provide: PrismaService, useValue: prisma },
            ],
        }).compile();

        service = module.get<BlogService>(BlogService);
    });

    it('should be defined', () => {
        expect(service).toBeDefined();
    });

    // ═══════════════════════════════════
    // findPublished
    // ═══════════════════════════════════
    describe('findPublished', () => {
        it('should return published articles with pagination', async () => {
            const mockArticles = [
                { id: '1', title: 'Art 1', status: 'PUBLISHED' },
                { id: '2', title: 'Art 2', status: 'PUBLISHED' },
            ];
            prisma.article.findMany.mockResolvedValue(mockArticles);
            prisma.article.count.mockResolvedValue(2);

            const result = await service.findPublished(undefined, 20, 0);

            expect(result).toEqual({ articles: mockArticles, total: 2 });
            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({
                    where: { status: 'PUBLISHED' },
                    take: 20,
                    skip: 0,
                })
            );
        });

        it('should filter by category when provided', async () => {
            prisma.article.findMany.mockResolvedValue([]);
            prisma.article.count.mockResolvedValue(0);

            await service.findPublished('IMMIGRATION', 10, 0);

            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({
                    where: { status: 'PUBLISHED', category: 'IMMIGRATION' },
                })
            );
        });

        it('should NOT filter by category when ALL is passed', async () => {
            prisma.article.findMany.mockResolvedValue([]);
            prisma.article.count.mockResolvedValue(0);

            await service.findPublished('ALL', 20, 0);

            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({
                    where: { status: 'PUBLISHED' },
                })
            );
        });
    });

    // ═══════════════════════════════════
    // findBySlug
    // ═══════════════════════════════════
    describe('findBySlug', () => {
        it('should return a published article and increment view count', async () => {
            const mockArticle = {
                id: '1', title: 'Test', slug: 'test', status: 'PUBLISHED',
                comments: [],
            };
            prisma.article.findUnique.mockResolvedValue(mockArticle);
            prisma.article.update.mockResolvedValue(mockArticle);

            const result = await service.findBySlug('test');

            expect(result).toEqual(mockArticle);
            expect(prisma.article.update).toHaveBeenCalledWith({
                where: { id: '1' },
                data: { viewCount: { increment: 1 } },
            });
        });

        it('should throw NotFoundException if article not found', async () => {
            prisma.article.findUnique.mockResolvedValue(null);

            await expect(service.findBySlug('nonexistent')).rejects.toThrow(NotFoundException);
        });

        it('should throw NotFoundException if article is not PUBLISHED', async () => {
            prisma.article.findUnique.mockResolvedValue({
                id: '1', title: 'Draft', slug: 'draft', status: 'DRAFT',
            });

            await expect(service.findBySlug('draft')).rejects.toThrow(NotFoundException);
        });
    });

    // ═══════════════════════════════════
    // findFeatured
    // ═══════════════════════════════════
    describe('findFeatured', () => {
        it('should return up to 3 published featured articles', async () => {
            const mockFeatured = [{ id: '1', featured: true, status: 'PUBLISHED' }];
            prisma.article.findMany.mockResolvedValue(mockFeatured);

            const result = await service.findFeatured();

            expect(result).toEqual(mockFeatured);
            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({
                    where: { status: 'PUBLISHED', featured: true },
                    take: 3,
                })
            );
        });
    });

    // ═══════════════════════════════════
    // create
    // ═══════════════════════════════════
    describe('create', () => {
        it('should create an article with generated slug and calculated readTime', async () => {
            prisma.article.findUnique.mockResolvedValue(null); // no existing slug
            const mockCreated = { id: '1', title: 'Mon Article', slug: 'mon-article' };
            prisma.article.create.mockResolvedValue(mockCreated);

            const result = await service.create({
                title: 'Mon Article',
                content: 'Un contenu avec suffisamment de mots pour calculer le temps de lecture correct.',
            });

            expect(result).toEqual(mockCreated);
            expect(prisma.article.create).toHaveBeenCalledWith({
                data: expect.objectContaining({
                    title: 'Mon Article',
                    slug: 'mon-article',
                    status: 'DRAFT',
                }),
            });
        });

        it('should set publishedAt if status is PUBLISHED', async () => {
            prisma.article.findUnique.mockResolvedValue(null);
            prisma.article.create.mockResolvedValue({ id: '1' });

            await service.create({
                title: 'Published Article',
                content: 'Content here.',
                status: 'PUBLISHED',
            });

            expect(prisma.article.create).toHaveBeenCalledWith({
                data: expect.objectContaining({
                    status: 'PUBLISHED',
                    publishedAt: expect.any(Date),
                }),
            });
        });

        it('should generate unique slug when slug already exists', async () => {
            prisma.article.findUnique.mockResolvedValue({ id: 'existing' }); // slug already taken
            prisma.article.create.mockResolvedValue({ id: '2' });

            await service.create({ title: 'Duplicate Title', content: 'Some content' });

            expect(prisma.article.create).toHaveBeenCalledWith({
                data: expect.objectContaining({
                    slug: expect.stringMatching(/^duplicate-title-\d+$/),
                }),
            });
        });
    });

    // ═══════════════════════════════════
    // update
    // ═══════════════════════════════════
    describe('update', () => {
        it('should update an existing article', async () => {
            prisma.article.findUnique.mockResolvedValue({
                id: '1', title: 'Old Title', status: 'DRAFT', publishedAt: null,
                comments: [],
            });
            prisma.article.update.mockResolvedValue({ id: '1', title: 'New Title' });

            const result = await service.update('1', { title: 'New Title' });

            expect(result).toEqual({ id: '1', title: 'New Title' });
        });

        it('should set publishedAt when status changes to PUBLISHED and was not already set', async () => {
            prisma.article.findUnique
                .mockResolvedValueOnce({ id: '1', publishedAt: null, comments: [] }) // findOne call
                .mockResolvedValueOnce({ id: '1', publishedAt: null }); // check for publishedAt
            prisma.article.update.mockResolvedValue({ id: '1' });

            await service.update('1', { status: 'PUBLISHED' });

            expect(prisma.article.update).toHaveBeenCalledWith({
                where: { id: '1' },
                data: expect.objectContaining({
                    status: 'PUBLISHED',
                    publishedAt: expect.any(Date),
                }),
            });
        });

        it('should throw NotFoundException if article does not exist', async () => {
            prisma.article.findUnique.mockResolvedValue(null);

            await expect(service.update('nonexistent', { title: 'X' })).rejects.toThrow(NotFoundException);
        });
    });

    // ═══════════════════════════════════
    // remove
    // ═══════════════════════════════════
    describe('remove', () => {
        it('should delete an article', async () => {
            prisma.article.findUnique.mockResolvedValue({ id: '1', comments: [] });
            prisma.article.delete.mockResolvedValue({ id: '1' });

            const result = await service.remove('1');

            expect(result).toEqual({ success: true });
            expect(prisma.article.delete).toHaveBeenCalledWith({ where: { id: '1' } });
        });

        it('should throw NotFoundException if article doesnt exist', async () => {
            prisma.article.findUnique.mockResolvedValue(null);

            await expect(service.remove('nonexistent')).rejects.toThrow(NotFoundException);
        });
    });

    // ═══════════════════════════════════
    // addComment
    // ═══════════════════════════════════
    describe('addComment', () => {
        it('should create a comment for a published article', async () => {
            prisma.article.findUnique.mockResolvedValue({ id: 'art1', status: 'PUBLISHED' });
            prisma.articleComment.count.mockResolvedValue(0); // no previous comments
            const mockComment = { id: 'c1', authorName: 'Jean', content: 'Bien !' };
            prisma.articleComment.create.mockResolvedValue(mockComment);

            const result = await service.addComment('test-slug', {
                authorName: 'Jean',
                content: 'Bien !',
            });

            expect(result).toEqual(mockComment);
        });

        it('should throw NotFoundException if article slug doesnt exist', async () => {
            prisma.article.findUnique.mockResolvedValue(null);

            await expect(service.addComment('invalid-slug', {
                authorName: 'Test',
                content: 'Hello',
            })).rejects.toThrow(NotFoundException);
        });

        it('should silently reject if honeypot field is filled (spam)', async () => {
            prisma.article.findUnique.mockResolvedValue({ id: 'art1', status: 'PUBLISHED' });

            const result = await service.addComment('test-slug', {
                authorName: 'Bot',
                content: 'spam',
                honeypot: 'filled-by-bot',
            });

            // Silent fail: returns fake success without creating a real comment
            expect(result).toEqual({ id: 'fake', success: true });
            expect(prisma.articleComment.create).not.toHaveBeenCalled();
        });

        it('should reject if rate limit is exceeded (3 comments per hour)', async () => {
            prisma.article.findUnique.mockResolvedValue({ id: 'art1', status: 'PUBLISHED' });
            prisma.articleComment.count.mockResolvedValue(3); // already 3 comments

            await expect(service.addComment('test-slug', {
                authorName: 'Jean',
                authorEmail: 'jean@test.com',
                content: 'Another comment',
            })).rejects.toThrow(BadRequestException);
        });
    });

    // ═══════════════════════════════════
    // Comments moderation
    // ═══════════════════════════════════
    describe('moderateComment', () => {
        it('should approve a comment', async () => {
            prisma.articleComment.update.mockResolvedValue({ id: 'c1', status: 'APPROVED' });

            const result = await service.moderateComment('c1', 'APPROVED');

            expect(prisma.articleComment.update).toHaveBeenCalledWith({
                where: { id: 'c1' },
                data: { status: 'APPROVED' },
            });
            expect(result.status).toBe('APPROVED');
        });

        it('should reject a comment', async () => {
            prisma.articleComment.update.mockResolvedValue({ id: 'c1', status: 'REJECTED' });

            const result = await service.moderateComment('c1', 'REJECTED');

            expect(result.status).toBe('REJECTED');
        });
    });

    describe('deleteComment', () => {
        it('should delete a comment', async () => {
            prisma.articleComment.delete.mockResolvedValue({ id: 'c1' });

            const result = await service.deleteComment('c1');

            expect(result).toEqual({ success: true });
        });
    });

    describe('getPendingComments', () => {
        it('should return pending comments ordered by createdAt', async () => {
            const mockPending = [
                { id: 'c1', status: 'PENDING', article: { title: 'Art 1', slug: 'art-1' } },
            ];
            prisma.articleComment.findMany.mockResolvedValue(mockPending);

            const result = await service.getPendingComments();

            expect(result).toEqual(mockPending);
            expect(prisma.articleComment.findMany).toHaveBeenCalledWith(
                expect.objectContaining({
                    where: { status: 'PENDING' },
                })
            );
        });
    });

    // ═══════════════════════════════════
    // generateRssFeed
    // ═══════════════════════════════════
    describe('generateRssFeed', () => {
        it('should generate valid RSS XML with articles', async () => {
            prisma.article.findMany.mockResolvedValue([
                {
                    title: 'Article Test',
                    slug: 'article-test',
                    excerpt: 'Un résumé.',
                    authorName: 'Admin',
                    category: 'GENERAL',
                    publishedAt: new Date('2026-01-01'),
                },
            ]);

            const rss = await service.generateRssFeed('https://simulegal.fr');

            expect(rss).toContain('<?xml version="1.0"');
            expect(rss).toContain('<title>Article Test</title>');
            expect(rss).toContain('<link>https://simulegal.fr/blog/article-test</link>');
            expect(rss).toContain('<description>Un résumé.</description>');
            expect(rss).toContain('<rss version="2.0"');
        });

        it('should return valid RSS XML even with no articles', async () => {
            prisma.article.findMany.mockResolvedValue([]);

            const rss = await service.generateRssFeed('https://simulegal.fr');

            expect(rss).toContain('<?xml version="1.0"');
            expect(rss).toContain('<channel>');
        });
    });

    // ═══════════════════════════════════
    // generateSitemap
    // ═══════════════════════════════════
    describe('generateSitemap', () => {
        it('should generate a valid sitemap XML', async () => {
            prisma.article.findMany.mockResolvedValue([
                {
                    slug: 'article-1',
                    updatedAt: new Date('2026-01-15'),
                    publishedAt: new Date('2026-01-01'),
                },
            ]);

            const sitemap = await service.generateSitemap('https://simulegal.fr');

            expect(sitemap).toContain('<?xml version="1.0"');
            expect(sitemap).toContain('<urlset');
            expect(sitemap).toContain('<loc>https://simulegal.fr/blog/article-1</loc>');
            expect(sitemap).toContain('<loc>https://simulegal.fr</loc>');
            expect(sitemap).toContain('<loc>https://simulegal.fr/blog</loc>');
        });
    });

    // ═══════════════════════════════════
    // findAll (admin)
    // ═══════════════════════════════════
    describe('findAll', () => {
        it('should return all articles when no status filter', async () => {
            prisma.article.findMany.mockResolvedValue([{ id: '1' }, { id: '2' }]);

            const result = await service.findAll();

            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({ where: {} })
            );
            expect(result).toHaveLength(2);
        });

        it('should filter by status when provided', async () => {
            prisma.article.findMany.mockResolvedValue([{ id: '1', status: 'DRAFT' }]);

            await service.findAll('DRAFT');

            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({ where: { status: 'DRAFT' } })
            );
        });

        it('should not filter by status when ALL is passed', async () => {
            prisma.article.findMany.mockResolvedValue([]);

            await service.findAll('ALL');

            expect(prisma.article.findMany).toHaveBeenCalledWith(
                expect.objectContaining({ where: {} })
            );
        });
    });
});
