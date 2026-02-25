import { Test, TestingModule } from '@nestjs/testing';
import { BlogController } from './blog.controller';
import { BlogService } from './blog.service';

const mockBlogService = () => ({
    findPublished: jest.fn(),
    findFeatured: jest.fn(),
    findBySlug: jest.fn(),
    addComment: jest.fn(),
    findAll: jest.fn(),
    findOne: jest.fn(),
    create: jest.fn(),
    update: jest.fn(),
    remove: jest.fn(),
    moderateComment: jest.fn(),
    deleteComment: jest.fn(),
    getPendingComments: jest.fn(),
    generateRssFeed: jest.fn(),
    generateSitemap: jest.fn(),
});

describe('BlogController', () => {
    let controller: BlogController;
    let blogService: ReturnType<typeof mockBlogService>;

    beforeEach(async () => {
        blogService = mockBlogService();
        const module: TestingModule = await Test.createTestingModule({
            controllers: [BlogController],
            providers: [{ provide: BlogService, useValue: blogService }],
        }).compile();

        controller = module.get<BlogController>(BlogController);
    });

    it('should be defined', () => {
        expect(controller).toBeDefined();
    });

    // ═══════════════════════════════════
    // Public endpoints
    // ═══════════════════════════════════
    describe('getPublished', () => {
        it('should call findPublished with parsed params', async () => {
            blogService.findPublished.mockResolvedValue({ articles: [], total: 0 });

            await controller.getPublished('IMMIGRATION', '10', '5');

            expect(blogService.findPublished).toHaveBeenCalledWith('IMMIGRATION', 10, 5);
        });

        it('should use defaults when no params provided', async () => {
            blogService.findPublished.mockResolvedValue({ articles: [], total: 0 });

            await controller.getPublished(undefined, undefined, undefined);

            expect(blogService.findPublished).toHaveBeenCalledWith(undefined, 20, 0);
        });
    });

    describe('getFeatured', () => {
        it('should return featured articles', async () => {
            const featured = [{ id: '1', featured: true }];
            blogService.findFeatured.mockResolvedValue(featured);

            const result = await controller.getFeatured();

            expect(result).toEqual(featured);
        });
    });

    describe('getBySlug', () => {
        it('should return article by slug', async () => {
            const article = { id: '1', slug: 'test' };
            blogService.findBySlug.mockResolvedValue(article);

            const result = await controller.getBySlug('test');

            expect(result).toEqual(article);
            expect(blogService.findBySlug).toHaveBeenCalledWith('test');
        });
    });

    describe('getRssFeed', () => {
        it('should return RSS XML', async () => {
            blogService.generateRssFeed.mockResolvedValue('<rss>...</rss>');

            const result = await controller.getRssFeed();

            expect(result).toContain('<rss');
        });
    });

    describe('getSitemap', () => {
        it('should return sitemap XML', async () => {
            blogService.generateSitemap.mockResolvedValue('<urlset>...</urlset>');

            const result = await controller.getSitemap();

            expect(result).toContain('<urlset');
        });
    });

    // ═══════════════════════════════════
    // Comments
    // ═══════════════════════════════════
    describe('addComment', () => {
        it('should pass comment data to service', async () => {
            const commentData = { authorName: 'Jean', content: 'Super article !' };
            blogService.addComment.mockResolvedValue({ id: 'c1', ...commentData });

            await controller.addComment('test-slug', commentData);

            expect(blogService.addComment).toHaveBeenCalledWith('test-slug', commentData);
        });
    });

    describe('moderateComment', () => {
        it('should pass moderation to service', async () => {
            blogService.moderateComment.mockResolvedValue({ id: 'c1', status: 'APPROVED' });

            await controller.moderateComment('c1', { status: 'APPROVED' });

            expect(blogService.moderateComment).toHaveBeenCalledWith('c1', 'APPROVED');
        });
    });

    describe('deleteComment', () => {
        it('should call service to delete comment', async () => {
            blogService.deleteComment.mockResolvedValue({ success: true });

            await controller.deleteComment('c1');

            expect(blogService.deleteComment).toHaveBeenCalledWith('c1');
        });
    });

    // ═══════════════════════════════════
    // Admin CRUD
    // ═══════════════════════════════════
    describe('adminList', () => {
        it('should call findAll with status', async () => {
            blogService.findAll.mockResolvedValue([]);

            await controller.adminList('PUBLISHED');

            expect(blogService.findAll).toHaveBeenCalledWith('PUBLISHED');
        });
    });

    describe('create', () => {
        it('should create an article', async () => {
            const data = { title: 'New', content: 'Content' };
            blogService.create.mockResolvedValue({ id: '1', ...data });

            const result = await controller.create(data as any);

            expect(result).toEqual({ id: '1', ...data });
            expect(blogService.create).toHaveBeenCalledWith(data);
        });
    });

    describe('update', () => {
        it('should update an article', async () => {
            blogService.update.mockResolvedValue({ id: '1', title: 'Updated' });

            const result = await controller.update('1', { title: 'Updated' });

            expect(result).toEqual({ id: '1', title: 'Updated' });
        });
    });

    describe('remove', () => {
        it('should delete an article', async () => {
            blogService.remove.mockResolvedValue({ success: true });

            const result = await controller.remove('1');

            expect(result).toEqual({ success: true });
        });
    });
});
