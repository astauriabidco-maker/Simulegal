'use client';

import React, { useState, useEffect } from 'react';
import Link from 'next/link';
import {
    ArrowRight, Clock, Eye, Tag, Search, ChevronLeft,
    BookOpen, Newspaper, Star, ArrowLeft
} from 'lucide-react';
import { BlogStore, Article, ArticleCategory } from '../../services/BlogStore';

const CATEGORIES: { id: ArticleCategory | 'ALL'; label: string; icon: string }[] = [
    { id: 'ALL', label: 'Tout', icon: '📋' },
    { id: 'IMMIGRATION', label: 'Immigration', icon: '🌍' },
    { id: 'NATURALISATION', label: 'Naturalisation', icon: '🇫🇷' },
    { id: 'SEJOUR', label: 'Titre de séjour', icon: '📄' },
    { id: 'PERMIS', label: 'Permis', icon: '🚗' },
    { id: 'FAMILY', label: 'Famille', icon: '👨‍👩‍👧‍👦' },
    { id: 'GENERAL', label: 'Général', icon: '📰' },
];

const CATEGORY_COLORS: Record<string, { bg: string; text: string; border: string }> = {
    IMMIGRATION: { bg: 'bg-emerald-50', text: 'text-emerald-700', border: 'border-emerald-200' },
    NATURALISATION: { bg: 'bg-blue-50', text: 'text-blue-700', border: 'border-blue-200' },
    SEJOUR: { bg: 'bg-indigo-50', text: 'text-indigo-700', border: 'border-indigo-200' },
    PERMIS: { bg: 'bg-amber-50', text: 'text-amber-700', border: 'border-amber-200' },
    FAMILY: { bg: 'bg-rose-50', text: 'text-rose-700', border: 'border-rose-200' },
    GENERAL: { bg: 'bg-slate-50', text: 'text-slate-600', border: 'border-slate-200' },
};

export default function BlogPage() {
    const [articles, setArticles] = useState<Article[]>([]);
    const [featured, setFeatured] = useState<Article[]>([]);
    const [isLoading, setIsLoading] = useState(true);
    const [filterCategory, setFilterCategory] = useState<string>('ALL');
    const [searchQuery, setSearchQuery] = useState('');
    const [total, setTotal] = useState(0);

    useEffect(() => {
        const load = async () => {
            setIsLoading(true);
            const [result, feat] = await Promise.all([
                BlogStore.getPublished(filterCategory, 50),
                BlogStore.getFeatured(),
            ]);
            setArticles(result.articles);
            setTotal(result.total);
            setFeatured(feat);
            setIsLoading(false);
        };
        load();
    }, [filterCategory]);

    const filteredArticles = articles.filter(a =>
        a.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
        (a.excerpt || '').toLowerCase().includes(searchQuery.toLowerCase()) ||
        (a.tags || '').toLowerCase().includes(searchQuery.toLowerCase())
    );

    // Separate featured from regular in the list view
    const regularArticles = filteredArticles.filter(a => !a.featured);
    const featuredInList = filteredArticles.filter(a => a.featured);

    const getCatColor = (cat: string) => CATEGORY_COLORS[cat] || CATEGORY_COLORS.GENERAL;
    const getCatLabel = (cat: string) => CATEGORIES.find(c => c.id === cat)?.label || cat;
    const getCatIcon = (cat: string) => CATEGORIES.find(c => c.id === cat)?.icon || '📰';

    return (
        <div className="min-h-screen bg-gradient-to-b from-slate-50 to-white">
            {/* ── Header ── */}
            <header className="bg-gradient-to-br from-indigo-600 via-indigo-700 to-purple-700 text-white">
                <div className="max-w-6xl mx-auto px-6 py-16 md:py-24">
                    <div className="flex items-center gap-3 mb-4">
                        <Link href="/" className="flex items-center gap-2 text-white/60 hover:text-white text-sm font-bold transition-colors">
                            <ArrowLeft size={16} />
                            Accueil
                        </Link>
                    </div>
                    <div className="flex items-center gap-4 mb-4">
                        <div className="w-14 h-14 bg-white/10 backdrop-blur-sm rounded-2xl flex items-center justify-center">
                            <Newspaper size={28} className="text-white" />
                        </div>
                        <div>
                            <h1 className="text-4xl md:text-5xl font-black tracking-tight">
                                Blog & Insights
                            </h1>
                        </div>
                    </div>
                    <p className="text-lg text-white/70 max-w-2xl font-medium leading-relaxed">
                        Décryptages juridiques, guides pratiques et actualités du droit des étrangers en France.
                        Restez informé des dernières évolutions réglementaires.
                    </p>

                    {/* Search */}
                    <div className="mt-8 max-w-lg">
                        <div className="relative">
                            <Search className="absolute left-4 top-1/2 -translate-y-1/2 text-white/40" size={20} />
                            <input
                                type="text"
                                placeholder="Rechercher un sujet..."
                                value={searchQuery}
                                onChange={(e) => setSearchQuery(e.target.value)}
                                className="w-full pl-12 pr-4 py-4 bg-white/10 backdrop-blur-sm border border-white/20 rounded-2xl text-white placeholder:text-white/40 font-medium focus:ring-2 focus:ring-white/30 outline-none transition-all"
                            />
                        </div>
                    </div>
                </div>
            </header>

            {/* ── Category Filters ── */}
            <div className="sticky top-0 z-30 bg-white/80 backdrop-blur-xl border-b border-slate-100 shadow-sm">
                <div className="max-w-6xl mx-auto px-6">
                    <div className="flex items-center gap-2 py-4 overflow-x-auto scrollbar-hide">
                        {CATEGORIES.map(cat => (
                            <button
                                key={cat.id}
                                onClick={() => setFilterCategory(cat.id)}
                                className={`px-4 py-2 rounded-full text-sm font-bold whitespace-nowrap transition-all ${filterCategory === cat.id
                                    ? 'bg-indigo-600 text-white shadow-lg shadow-indigo-200'
                                    : 'bg-slate-100 text-slate-500 hover:bg-slate-200'
                                    }`}
                            >
                                {cat.icon} {cat.label}
                            </button>
                        ))}
                    </div>
                </div>
            </div>

            <main className="max-w-6xl mx-auto px-6 py-12">
                {isLoading ? (
                    <div className="flex flex-col items-center justify-center py-32 gap-4">
                        <div className="w-10 h-10 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin" />
                        <p className="text-slate-400 font-bold text-sm">Chargement des articles...</p>
                    </div>
                ) : filteredArticles.length === 0 ? (
                    <div className="text-center py-32">
                        <BookOpen size={56} className="mx-auto text-slate-200 mb-6" />
                        <h2 className="text-2xl font-black text-slate-800 mb-3">Aucun article trouvé</h2>
                        <p className="text-slate-400 font-medium">Essayez une autre catégorie ou un autre mot-clé.</p>
                    </div>
                ) : (
                    <>
                        {/* ── Featured Hero ── */}
                        {filterCategory === 'ALL' && !searchQuery && featuredInList.length > 0 && (
                            <section className="mb-16">
                                <div className="flex items-center gap-2 mb-6">
                                    <Star size={18} className="text-amber-500 fill-amber-500" />
                                    <h2 className="text-lg font-black text-slate-800 uppercase tracking-wider">En vedette</h2>
                                </div>
                                <div className="grid md:grid-cols-2 gap-6">
                                    {featuredInList.map(article => {
                                        const catColor = getCatColor(article.category);
                                        return (
                                            <Link
                                                key={article.id}
                                                href={`/blog/${article.slug}`}
                                                className="group relative bg-white rounded-3xl border border-slate-100 overflow-hidden shadow-sm hover:shadow-xl transition-all duration-300 hover:-translate-y-1"
                                            >
                                                {/* Gradient bar */}
                                                <div className="h-2 bg-gradient-to-r from-indigo-500 via-purple-500 to-pink-500" />

                                                <div className="p-8">
                                                    <div className="flex items-center gap-3 mb-4">
                                                        <span className={`px-3 py-1 rounded-full text-[11px] font-black uppercase tracking-wider ${catColor.bg} ${catColor.text}`}>
                                                            {getCatIcon(article.category)} {getCatLabel(article.category)}
                                                        </span>
                                                        <Star size={14} className="text-amber-400 fill-amber-400" />
                                                    </div>

                                                    <h3 className="text-2xl font-black text-slate-900 mb-3 group-hover:text-indigo-600 transition-colors leading-tight">
                                                        {article.title}
                                                    </h3>

                                                    {article.excerpt && (
                                                        <p className="text-slate-500 font-medium text-sm leading-relaxed mb-6 line-clamp-3">
                                                            {article.excerpt}
                                                        </p>
                                                    )}

                                                    <div className="flex items-center justify-between">
                                                        <div className="flex items-center gap-4 text-xs text-slate-400 font-bold">
                                                            <span className="flex items-center gap-1"><Clock size={12} /> {article.readTimeMin} min</span>
                                                            <span>{article.authorName}</span>
                                                            {article.publishedAt && (
                                                                <span>{new Date(article.publishedAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'short', year: 'numeric' })}</span>
                                                            )}
                                                        </div>
                                                        <span className="text-indigo-600 font-bold text-sm flex items-center gap-1 group-hover:gap-2 transition-all">
                                                            Lire <ArrowRight size={14} />
                                                        </span>
                                                    </div>
                                                </div>
                                            </Link>
                                        );
                                    })}
                                </div>
                            </section>
                        )}

                        {/* ── Articles Grid ── */}
                        <section>
                            {filterCategory === 'ALL' && !searchQuery && regularArticles.length > 0 && (
                                <div className="flex items-center gap-2 mb-6">
                                    <Newspaper size={18} className="text-slate-400" />
                                    <h2 className="text-lg font-black text-slate-800 uppercase tracking-wider">Tous les articles</h2>
                                    <span className="text-xs font-bold text-slate-400 bg-slate-100 px-2 py-0.5 rounded-full">{total}</span>
                                </div>
                            )}

                            <div className="grid md:grid-cols-3 gap-6">
                                {(searchQuery || filterCategory !== 'ALL' ? filteredArticles : regularArticles).map(article => {
                                    const catColor = getCatColor(article.category);
                                    return (
                                        <Link
                                            key={article.id}
                                            href={`/blog/${article.slug}`}
                                            className="group bg-white rounded-2xl border border-slate-100 overflow-hidden shadow-sm hover:shadow-lg transition-all duration-300 hover:-translate-y-1 flex flex-col"
                                        >
                                            {/* Category colored top */}
                                            <div className={`h-1.5 ${catColor.bg.replace('50', '400')}`} style={{
                                                background: article.category === 'NATURALISATION' ? '#3b82f6' :
                                                    article.category === 'SEJOUR' ? '#6366f1' :
                                                        article.category === 'PERMIS' ? '#f59e0b' :
                                                            article.category === 'IMMIGRATION' ? '#10b981' :
                                                                article.category === 'FAMILY' ? '#f43f5e' : '#94a3b8'
                                            }} />

                                            <div className="p-6 flex-1 flex flex-col">
                                                <div className="flex items-center gap-2 mb-3">
                                                    <span className={`px-2.5 py-0.5 rounded-md text-[10px] font-black uppercase tracking-wider ${catColor.bg} ${catColor.text}`}>
                                                        {getCatIcon(article.category)} {getCatLabel(article.category)}
                                                    </span>
                                                </div>

                                                <h3 className="text-lg font-black text-slate-900 mb-2 group-hover:text-indigo-600 transition-colors leading-snug line-clamp-2">
                                                    {article.title}
                                                </h3>

                                                {article.excerpt && (
                                                    <p className="text-sm text-slate-400 font-medium leading-relaxed mb-4 line-clamp-3 flex-1">
                                                        {article.excerpt}
                                                    </p>
                                                )}

                                                <div className="flex items-center justify-between mt-auto pt-4 border-t border-slate-50">
                                                    <div className="flex items-center gap-3 text-[11px] text-slate-400 font-bold">
                                                        <span className="flex items-center gap-1"><Clock size={11} /> {article.readTimeMin} min</span>
                                                        {article.publishedAt && (
                                                            <span>{new Date(article.publishedAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'short' })}</span>
                                                        )}
                                                    </div>
                                                    <ArrowRight size={16} className="text-slate-300 group-hover:text-indigo-500 group-hover:translate-x-1 transition-all" />
                                                </div>
                                            </div>
                                        </Link>
                                    );
                                })}
                            </div>
                        </section>

                        {/* ── CTA Bottom ── */}
                        <section className="mt-20 text-center bg-gradient-to-br from-indigo-50 via-purple-50 to-pink-50 rounded-3xl p-12 border border-indigo-100">
                            <h2 className="text-2xl font-black text-slate-900 mb-3">Vérifiez votre éligibilité gratuitement</h2>
                            <p className="text-slate-500 font-medium mb-6 max-w-lg mx-auto">
                                Notre simulateur intelligent analyse votre profil et vous indique les démarches possibles en quelques minutes.
                            </p>
                            <Link
                                href="/"
                                className="inline-flex items-center gap-2 px-8 py-4 bg-indigo-600 hover:bg-indigo-700 text-white font-black rounded-2xl transition-all shadow-lg shadow-indigo-200 hover:shadow-xl"
                            >
                                Lancer le simulateur
                                <ArrowRight size={20} />
                            </Link>
                        </section>
                    </>
                )}
            </main>

            {/* ── Footer ── */}
            <footer className="bg-slate-900 py-12 px-6 text-white/40 text-center text-xs font-bold uppercase tracking-widest mt-20 flex flex-col items-center justify-center gap-4">
                <div className="flex items-center gap-6">
                    <a href="/public/blog/rss" target="_blank" rel="noopener noreferrer" className="hover:text-amber-500 transition-colors">RSS Feed</a>
                    <a href="/public/blog/sitemap.xml" target="_blank" rel="noopener noreferrer" className="hover:text-emerald-500 transition-colors">Sitemap</a>
                </div>
                <p>© 2026 SimuLegal — Blog & Insights Juridiques</p>
            </footer>
        </div>
    );
}
