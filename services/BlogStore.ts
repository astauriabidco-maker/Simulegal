/**
 * BlogStore – API client for the Blog module
 * Handles articles, comments, reactions, newsletter, analytics, categories, revisions, export/import
 */

const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

export interface Article {
    id: string; title: string; slug: string; excerpt?: string;
    content: string; category: string; status: string;
    coverImage?: string; authorName: string; authorRole?: string;
    tags?: string; metaTitle?: string; metaDescription?: string;
    readTimeMin: number; viewCount: number; featured: boolean;
    scheduledAt?: string; publishedAt?: string;
    createdAt: string; updatedAt: string;
    _count?: { comments: number; reactions: number };
    reactionCounts?: Record<string, number>;
}

export interface ArticleComment {
    id: string; articleId: string; authorName: string; authorEmail?: string;
    content: string; status: string; parentId?: string; createdAt: string;
    article?: { title: string; slug: string };
}

export interface ArticleRevision {
    id: string; articleId: string; title: string; content?: string;
    excerpt?: string; editedBy?: string; version: number; createdAt: string;
}

export interface BlogCategory {
    id: string; name: string; slug: string;
    icon?: string; color?: string; sortOrder: number; createdAt: string;
}

export type ArticleCategory = 'IMMIGRATION' | 'NATURALISATION' | 'SEJOUR' | 'PERMIS' | 'FAMILY' | 'GENERAL';
export type ArticleStatus = 'DRAFT' | 'PUBLISHED' | 'ARCHIVED' | 'SCHEDULED';

function getAuthHeaders(): HeadersInit {
    const token = typeof window !== 'undefined' ? localStorage.getItem('admin_token') : null;
    return {
        'Content-Type': 'application/json',
        ...(token ? { Authorization: `Bearer ${token}` } : {}),
    };
}

export const BlogStore = {
    // ═══════════════════════════════════════════
    // PUBLIC – Articles
    // ═══════════════════════════════════════════

    getPublished: async (category?: string, limit = 20, offset = 0, search?: string, tag?: string): Promise<{ articles: Article[]; total: number; hasMore: boolean }> => {
        const params = new URLSearchParams();
        if (category) params.set('category', category);
        if (limit) params.set('limit', String(limit));
        if (offset) params.set('offset', String(offset));
        if (search) params.set('search', search);
        if (tag) params.set('tag', tag);
        const res = await fetch(`${API}/public/blog?${params}`);
        if (!res.ok) return { articles: [], total: 0, hasMore: false };
        return res.json();
    },

    getFeatured: async (): Promise<Article[]> => {
        const res = await fetch(`${API}/public/blog/featured`);
        if (!res.ok) return [];
        return res.json();
    },

    getBySlug: async (slug: string): Promise<Article | null> => {
        const res = await fetch(`${API}/public/blog/${slug}`);
        if (!res.ok) return null;
        return res.json();
    },

    // ═══════════════════════════════════════════
    // PUBLIC – Comments
    // ═══════════════════════════════════════════

    postComment: async (slug: string, data: { authorName: string; authorEmail?: string; content: string; honeypot?: string; parentId?: string }): Promise<boolean> => {
        const res = await fetch(`${API}/public/blog/${slug}/comments`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(data),
        });
        return res.ok;
    },

    // ═══════════════════════════════════════════
    // PUBLIC – Reactions
    // ═══════════════════════════════════════════

    toggleReaction: async (articleId: string, type: string): Promise<{ toggled: 'added' | 'removed' } | null> => {
        const res = await fetch(`${API}/public/blog/${articleId}/reactions`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ type }),
        });
        if (!res.ok) return null;
        return res.json();
    },

    getReactions: async (articleId: string): Promise<Record<string, number>> => {
        const res = await fetch(`${API}/public/blog/${articleId}/reactions`);
        if (!res.ok) return {};
        return res.json();
    },

    // ═══════════════════════════════════════════
    // PUBLIC – Newsletter
    // ═══════════════════════════════════════════

    subscribe: async (email: string, name?: string): Promise<{ success: boolean; message: string }> => {
        const res = await fetch(`${API}/public/newsletter/subscribe`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ email, name }),
        });
        if (!res.ok) return { success: false, message: 'Erreur' };
        return res.json();
    },

    // ═══════════════════════════════════════════
    // ADMIN – Articles CRUD
    // ═══════════════════════════════════════════

    getAll: async (status?: string): Promise<Article[]> => {
        const res = await fetch(`${API}/blog${status ? `?status=${status}` : ''}`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    getOne: async (id: string): Promise<Article | null> => {
        const res = await fetch(`${API}/blog/${id}`, { headers: getAuthHeaders() });
        if (!res.ok) return null;
        return res.json();
    },

    create: async (data: Partial<Article>): Promise<Article | null> => {
        const res = await fetch(`${API}/blog`, {
            method: 'POST', headers: getAuthHeaders(), body: JSON.stringify(data),
        });
        if (!res.ok) return null;
        return res.json();
    },

    update: async (id: string, data: Partial<Article>): Promise<Article | null> => {
        const res = await fetch(`${API}/blog/${id}`, {
            method: 'PUT', headers: getAuthHeaders(), body: JSON.stringify(data),
        });
        if (!res.ok) return null;
        return res.json();
    },

    delete: async (id: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/${id}`, { method: 'DELETE', headers: getAuthHeaders() });
        return res.ok;
    },

    // ═══════════════════════════════════════════
    // ADMIN – Comments Moderation
    // ═══════════════════════════════════════════

    getPendingComments: async (): Promise<ArticleComment[]> => {
        const res = await fetch(`${API}/blog/comments/pending`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    moderateComment: async (commentId: string, status: 'APPROVED' | 'REJECTED'): Promise<boolean> => {
        const res = await fetch(`${API}/blog/comments/${commentId}`, {
            method: 'PATCH', headers: getAuthHeaders(), body: JSON.stringify({ status }),
        });
        return res.ok;
    },

    deleteComment: async (commentId: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/comments/${commentId}`, { method: 'DELETE', headers: getAuthHeaders() });
        return res.ok;
    },

    // ═══════════════════════════════════════════
    // ADMIN – Analytics
    // ═══════════════════════════════════════════

    getDashboardAnalytics: async (days = 30): Promise<{
        totalArticles: number; publishedCount: number; totalViews: number;
        featuredCount: number; totalComments: number; totalReactions: number;
        topArticles: { id: string; title: string; slug: string; viewCount: number; publishedAt: string }[];
        dailyViews: Record<string, number>;
    } | null> => {
        const res = await fetch(`${API}/blog/analytics/dashboard?days=${days}`, { headers: getAuthHeaders() });
        if (!res.ok) return null;
        return res.json();
    },

    getArticleAnalytics: async (articleId: string, days = 30): Promise<any[]> => {
        const res = await fetch(`${API}/blog/analytics/${articleId}?days=${days}`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    // ═══════════════════════════════════════════
    // ADMIN – Newsletter Management
    // ═══════════════════════════════════════════

    getSubscribers: async (): Promise<{ id: string; email: string; name?: string; subscribedAt: string }[]> => {
        const res = await fetch(`${API}/blog/newsletter/subscribers`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    getSubscriberStats: async (): Promise<{ active: number; total: number; unsubscribed: number }> => {
        const res = await fetch(`${API}/blog/newsletter/stats`, { headers: getAuthHeaders() });
        if (!res.ok) return { active: 0, total: 0, unsubscribed: 0 };
        return res.json();
    },

    // ═══════════════════════════════════════════
    // ADMIN – Categories
    // ═══════════════════════════════════════════

    getCategories: async (): Promise<BlogCategory[]> => {
        const res = await fetch(`${API}/blog/categories`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    createCategory: async (data: { name: string; icon?: string; color?: string }): Promise<BlogCategory | null> => {
        const res = await fetch(`${API}/blog/categories`, {
            method: 'POST', headers: getAuthHeaders(), body: JSON.stringify(data),
        });
        if (!res.ok) return null;
        return res.json();
    },

    updateCategory: async (id: string, data: Partial<BlogCategory>): Promise<boolean> => {
        const res = await fetch(`${API}/blog/categories/${id}`, {
            method: 'PUT', headers: getAuthHeaders(), body: JSON.stringify(data),
        });
        return res.ok;
    },

    deleteCategory: async (id: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/categories/${id}`, { method: 'DELETE', headers: getAuthHeaders() });
        return res.ok;
    },

    // ═══════════════════════════════════════════
    // ADMIN – Revisions
    // ═══════════════════════════════════════════

    getRevisions: async (articleId: string): Promise<ArticleRevision[]> => {
        const res = await fetch(`${API}/blog/${articleId}/revisions`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    getRevision: async (revisionId: string): Promise<ArticleRevision | null> => {
        const res = await fetch(`${API}/blog/revisions/${revisionId}`, { headers: getAuthHeaders() });
        if (!res.ok) return null;
        return res.json();
    },

    restoreRevision: async (articleId: string, revisionId: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/${articleId}/revisions/${revisionId}/restore`, {
            method: 'POST', headers: getAuthHeaders(),
        });
        return res.ok;
    },

    // ═══════════════════════════════════════════
    // ADMIN – Export / Import
    // ═══════════════════════════════════════════

    exportArticles: async (format: 'json' | 'markdown' = 'json'): Promise<any> => {
        const res = await fetch(`${API}/blog/export/${format}`, { headers: getAuthHeaders() });
        if (!res.ok) return null;
        return res.json();
    },

    importArticle: async (data: { title: string; content: string; slug?: string; category?: string; tags?: string }): Promise<Article | null> => {
        const res = await fetch(`${API}/blog/import`, {
            method: 'POST', headers: getAuthHeaders(), body: JSON.stringify(data),
        });
        if (!res.ok) return null;
        return res.json();
    },

    // ═══════════════════════════════════════════
    // Image Upload
    // ═══════════════════════════════════════════

    uploadImage: async (file: File): Promise<{ url: string; filename: string } | null> => {
        const token = typeof window !== 'undefined' ? localStorage.getItem('admin_token') : null;
        const formData = new FormData();
        formData.append('file', file);
        const res = await fetch(`${API}/blog/upload`, {
            method: 'POST',
            headers: token ? { Authorization: `Bearer ${token}` } : {},
            body: formData,
        });
        if (!res.ok) return null;
        return res.json();
    },

    listImages: async (): Promise<{ filename: string; url: string; size: number; createdAt: string }[]> => {
        const res = await fetch(`${API}/blog/upload/list/all`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    // ═══════════════════════════════════════════
    // ADMIN – Auto Veille Pipeline
    // ═══════════════════════════════════════════

    getAutoTopics: async (status?: string): Promise<BlogAutoTopic[]> => {
        const params = status ? `?status=${status}` : '';
        const res = await fetch(`${API}/blog/auto/topics${params}`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    getAutoStats: async (): Promise<BlogAutoStats | null> => {
        const res = await fetch(`${API}/blog/auto/stats`, { headers: getAuthHeaders() });
        if (!res.ok) return null;
        return res.json();
    },

    getAutoConfig: async (): Promise<BlogAutoConfig | null> => {
        const res = await fetch(`${API}/blog/auto/config`, { headers: getAuthHeaders() });
        if (!res.ok) return null;
        return res.json();
    },

    updateAutoConfig: async (data: Partial<BlogAutoConfig>): Promise<boolean> => {
        const res = await fetch(`${API}/blog/auto/config`, {
            method: 'PUT', headers: getAuthHeaders(), body: JSON.stringify(data),
        });
        return res.ok;
    },

    triggerPipeline: async (): Promise<{ discovered: number; generated: number; errors: number } | null> => {
        const res = await fetch(`${API}/blog/auto/trigger`, {
            method: 'POST', headers: getAuthHeaders(),
        });
        if (!res.ok) return null;
        return res.json();
    },

    discoverTopics: async (): Promise<{ discovered: number; sources: string[] } | null> => {
        const res = await fetch(`${API}/blog/auto/discover`, {
            method: 'POST', headers: getAuthHeaders(),
        });
        if (!res.ok) return null;
        return res.json();
    },

    generateDrafts: async (): Promise<{ generated: number; errors: number } | null> => {
        const res = await fetch(`${API}/blog/auto/generate`, {
            method: 'POST', headers: getAuthHeaders(),
        });
        if (!res.ok) return null;
        return res.json();
    },

    rejectTopic: async (id: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/auto/topics/${id}/reject`, {
            method: 'POST', headers: getAuthHeaders(),
        });
        return res.ok;
    },

    retryTopic: async (id: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/auto/topics/${id}/retry`, {
            method: 'POST', headers: getAuthHeaders(),
        });
        return res.ok;
    },
};

export interface BlogAutoTopic {
    id: string; title: string; summary: string; sourceUrl?: string; sourceName?: string;
    category: string; relevanceScore: number; keywords?: string; status: string;
    generatedArticleId?: string; legalUpdateId?: string; error?: string;
    createdAt: string; updatedAt: string;
}

export interface BlogAutoConfig {
    id: string; enabled: boolean; frequency: string; maxArticlesPerRun: number;
    minRelevanceScore: number; targetCategories: string; aiModel: string;
    aiPromptTemplate?: string; lastRunAt?: string; totalGenerated: number;
}

export interface BlogAutoStats {
    total: number; discovered: number; generating: number; generated: number;
    rejected: number; published: number;
    config: { enabled: boolean; frequency: string; lastRunAt?: string; totalGenerated: number };
}

export default BlogStore;
