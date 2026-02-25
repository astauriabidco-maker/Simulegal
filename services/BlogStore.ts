const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

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
};
