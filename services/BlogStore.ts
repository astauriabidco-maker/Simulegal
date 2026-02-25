const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

export interface ArticleComment {
    id: string;
    articleId: string;
    authorName: string;
    authorEmail?: string;
    content: string;
    status: 'PENDING' | 'APPROVED' | 'REJECTED';
    parentId?: string;
    createdAt: string;
    article?: { title: string; slug: string };
}

export interface Article {
    id: string;
    title: string;
    slug: string;
    excerpt: string | null;
    content: string;
    category: string;
    status: 'DRAFT' | 'PUBLISHED' | 'ARCHIVED';
    coverImage: string | null;
    authorName: string;
    authorRole: string | null;
    tags: string | null;
    metaTitle: string | null;
    metaDescription: string | null;
    readTimeMin: number;
    viewCount: number;
    featured: boolean;
    publishedAt: string | null;
    createdAt: string;
    updatedAt: string;
    comments?: ArticleComment[];
    _count?: { comments: number };
}

export type ArticleCategory = 'GENERAL' | 'IMMIGRATION' | 'NATURALISATION' | 'SEJOUR' | 'PERMIS' | 'FAMILY';
export type ArticleStatus = 'DRAFT' | 'PUBLISHED' | 'ARCHIVED';

function getAuthHeaders(): HeadersInit {
    const token = typeof window !== 'undefined' ? localStorage.getItem('token') : null;
    return {
        'Content-Type': 'application/json',
        ...(token ? { Authorization: `Bearer ${token}` } : {}),
    };
}

export const BlogStore = {
    // ── Public ──
    getPublished: async (category?: string, limit = 20, offset = 0): Promise<{ articles: Article[]; total: number }> => {
        const params = new URLSearchParams();
        if (category && category !== 'ALL') params.set('category', category);
        params.set('limit', limit.toString());
        params.set('offset', offset.toString());
        const res = await fetch(`${API}/public/blog?${params}`);
        if (!res.ok) return { articles: [], total: 0 };
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

    postComment: async (slug: string, data: { authorName: string; authorEmail?: string; content: string; honeypot?: string; parentId?: string }): Promise<boolean> => {
        const res = await fetch(`${API}/public/blog/${slug}/comments`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(data),
        });
        return res.ok;
    },

    // ── Admin ──
    getAll: async (status?: string): Promise<Article[]> => {
        const params = status ? `?status=${status}` : '';
        const res = await fetch(`${API}/blog${params}`, { headers: getAuthHeaders() });
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
            method: 'POST',
            headers: getAuthHeaders(),
            body: JSON.stringify(data),
        });
        if (!res.ok) return null;
        return res.json();
    },

    update: async (id: string, data: Partial<Article>): Promise<Article | null> => {
        const res = await fetch(`${API}/blog/${id}`, {
            method: 'PUT',
            headers: getAuthHeaders(),
            body: JSON.stringify(data),
        });
        if (!res.ok) return null;
        return res.json();
    },

    remove: async (id: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/${id}`, {
            method: 'DELETE',
            headers: getAuthHeaders(),
        });
        return res.ok;
    },

    // ── Admin Comments ──
    getPendingComments: async (): Promise<ArticleComment[]> => {
        const res = await fetch(`${API}/blog/comments/pending`, { headers: getAuthHeaders() });
        if (!res.ok) return [];
        return res.json();
    },

    moderateComment: async (commentId: string, status: 'APPROVED' | 'REJECTED'): Promise<boolean> => {
        const res = await fetch(`${API}/blog/comments/${commentId}`, {
            method: 'PATCH',
            headers: getAuthHeaders(),
            body: JSON.stringify({ status }),
        });
        return res.ok;
    },

    deleteComment: async (commentId: string): Promise<boolean> => {
        const res = await fetch(`${API}/blog/comments/${commentId}`, {
            method: 'DELETE',
            headers: getAuthHeaders(),
        });
        return res.ok;
    },

    // ── Image Upload ──
    uploadImage: async (file: File): Promise<{ url: string; filename: string } | null> => {
        const token = typeof window !== 'undefined' ? localStorage.getItem('token') : null;
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
};
