'use client';

import React, { useState, useEffect } from 'react';
import { useParams } from 'next/navigation';
import Link from 'next/link';
import {
    ArrowLeft, Clock, Eye, Calendar, Tag, User, Share2,
    BookOpen, Loader2, AlertCircle, ArrowRight, ChevronRight
} from 'lucide-react';
import { BlogStore, Article } from '../../../services/BlogStore';

const CATEGORY_LABELS: Record<string, string> = {
    IMMIGRATION: '🌍 Immigration',
    NATURALISATION: '🇫🇷 Naturalisation',
    SEJOUR: '📄 Titre de séjour',
    PERMIS: '🚗 Permis de conduire',
    FAMILY: '👨‍👩‍👧‍👦 Famille',
    GENERAL: '📰 Général',
};

const CATEGORY_COLORS: Record<string, string> = {
    IMMIGRATION: 'bg-emerald-100 text-emerald-700',
    NATURALISATION: 'bg-blue-100 text-blue-700',
    SEJOUR: 'bg-indigo-100 text-indigo-700',
    PERMIS: 'bg-amber-100 text-amber-700',
    FAMILY: 'bg-rose-100 text-rose-700',
    GENERAL: 'bg-slate-100 text-slate-600',
};

// Simple Markdown → HTML renderer (handles **, ##, -, >, 1.)
function renderMarkdown(md: string): string {
    return md
        // Headers
        .replace(/^### (.+)$/gm, '<h3 class="text-xl font-black text-slate-900 mt-8 mb-3">$1</h3>')
        .replace(/^## (.+)$/gm, '<h2 class="text-2xl font-black text-slate-900 mt-10 mb-4">$1</h2>')
        // Bold
        .replace(/\*\*(.+?)\*\*/g, '<strong class="font-bold text-slate-900">$1</strong>')
        // Blockquote
        .replace(/^> (.+)$/gm, '<blockquote class="border-l-4 border-indigo-400 pl-5 py-3 my-6 bg-indigo-50/50 rounded-r-xl text-slate-600 italic font-medium">$1</blockquote>')
        // Unordered list items
        .replace(/^- (.+)$/gm, '<li class="flex items-start gap-2 py-1"><span class="w-1.5 h-1.5 rounded-full bg-indigo-500 mt-2 shrink-0"></span><span>$1</span></li>')
        // Ordered list items
        .replace(/^(\d+)\. (.+)$/gm, '<li class="flex items-start gap-3 py-1.5"><span class="w-6 h-6 rounded-full bg-indigo-100 text-indigo-700 flex items-center justify-center text-xs font-black shrink-0">$1</span><span>$2</span></li>')
        // Paragraphs (double newlines)
        .replace(/\n\n/g, '</p><p class="text-slate-600 leading-relaxed mb-4 font-medium">')
        // Single newlines within list context — keep as-is
        .replace(/\n/g, '<br/>');
}

export default function BlogArticlePage() {
    const params = useParams();
    const slug = params.slug as string;

    const [article, setArticle] = useState<Article | null>(null);
    const [isLoading, setIsLoading] = useState(true);
    const [relatedArticles, setRelatedArticles] = useState<Article[]>([]);

    useEffect(() => {
        const load = async () => {
            setIsLoading(true);
            const art = await BlogStore.getBySlug(slug);
            setArticle(art);

            // Load related articles from same category
            if (art) {
                const { articles } = await BlogStore.getPublished(art.category, 4);
                setRelatedArticles(articles.filter(a => a.id !== art.id).slice(0, 3));
            }
            setIsLoading(false);
        };
        load();
    }, [slug]);

    if (isLoading) {
        return (
            <div className="min-h-screen bg-white flex items-center justify-center">
                <div className="text-center">
                    <Loader2 size={40} className="animate-spin text-indigo-600 mx-auto mb-4" />
                    <p className="text-slate-400 font-bold text-sm">Chargement de l&apos;article...</p>
                </div>
            </div>
        );
    }

    if (!article) {
        return (
            <div className="min-h-screen bg-white flex items-center justify-center">
                <div className="text-center max-w-md">
                    <AlertCircle size={56} className="text-slate-200 mx-auto mb-6" />
                    <h1 className="text-2xl font-black text-slate-800 mb-3">Article introuvable</h1>
                    <p className="text-slate-400 font-medium mb-6">Cet article n&apos;existe pas ou a été dépublié.</p>
                    <Link href="/blog" className="inline-flex items-center gap-2 text-indigo-600 font-bold hover:underline">
                        <ArrowLeft size={16} />
                        Retour au blog
                    </Link>
                </div>
            </div>
        );
    }

    const catLabel = CATEGORY_LABELS[article.category] || article.category;
    const catColor = CATEGORY_COLORS[article.category] || CATEGORY_COLORS.GENERAL;
    const tags = article.tags ? article.tags.split(',').map(t => t.trim()).filter(Boolean) : [];

    const handleShare = () => {
        if (navigator.share) {
            navigator.share({ title: article.title, url: window.location.href });
        } else {
            navigator.clipboard.writeText(window.location.href);
        }
    };

    return (
        <div className="min-h-screen bg-white">
            {/* ── Breadcrumb ── */}
            <div className="bg-slate-50 border-b border-slate-100">
                <div className="max-w-4xl mx-auto px-6 py-3 flex items-center gap-2 text-xs font-bold text-slate-400">
                    <Link href="/" className="hover:text-slate-600 transition-colors">Accueil</Link>
                    <ChevronRight size={12} />
                    <Link href="/blog" className="hover:text-slate-600 transition-colors">Blog</Link>
                    <ChevronRight size={12} />
                    <span className="text-slate-600 truncate max-w-[200px]">{article.title}</span>
                </div>
            </div>

            {/* ── Article Header ── */}
            <header className="max-w-4xl mx-auto px-6 pt-12 pb-8">
                <Link href="/blog" className="inline-flex items-center gap-2 text-slate-400 hover:text-indigo-600 font-bold text-sm transition-colors mb-8">
                    <ArrowLeft size={16} />
                    Retour au blog
                </Link>

                <div className="flex items-center gap-3 mb-5">
                    <span className={`px-3 py-1 rounded-full text-xs font-black uppercase tracking-wider ${catColor}`}>
                        {catLabel}
                    </span>
                </div>

                <h1 className="text-4xl md:text-5xl font-black text-slate-900 leading-tight mb-6">
                    {article.title}
                </h1>

                {article.excerpt && (
                    <p className="text-xl text-slate-500 font-medium leading-relaxed mb-8 max-w-3xl">
                        {article.excerpt}
                    </p>
                )}

                {/* Meta */}
                <div className="flex flex-wrap items-center gap-6 text-sm text-slate-400 font-bold border-t border-b border-slate-100 py-4">
                    <span className="flex items-center gap-2">
                        <User size={16} className="text-indigo-500" />
                        <span className="text-slate-700">{article.authorName}</span>
                        {article.authorRole && <span className="text-slate-300">• {article.authorRole}</span>}
                    </span>
                    {article.publishedAt && (
                        <span className="flex items-center gap-1.5">
                            <Calendar size={14} />
                            {new Date(article.publishedAt).toLocaleDateString('fr-FR', {
                                day: 'numeric', month: 'long', year: 'numeric'
                            })}
                        </span>
                    )}
                    <span className="flex items-center gap-1.5">
                        <Clock size={14} />
                        {article.readTimeMin} min de lecture
                    </span>
                    <span className="flex items-center gap-1.5">
                        <Eye size={14} />
                        {article.viewCount} vues
                    </span>
                    <button onClick={handleShare} className="ml-auto flex items-center gap-1.5 hover:text-indigo-600 transition-colors">
                        <Share2 size={14} />
                        Partager
                    </button>
                </div>
            </header>

            {/* ── Article Body ── */}
            <article className="max-w-4xl mx-auto px-6 py-8">
                <div
                    className="prose prose-lg max-w-none text-slate-600 leading-relaxed font-medium"
                    dangerouslySetInnerHTML={{ __html: `<p class="text-slate-600 leading-relaxed mb-4 font-medium">${renderMarkdown(article.content)}</p>` }}
                />

                {/* Tags */}
                {tags.length > 0 && (
                    <div className="mt-12 pt-8 border-t border-slate-100">
                        <div className="flex items-center gap-2 mb-3">
                            <Tag size={16} className="text-slate-400" />
                            <span className="text-xs font-black text-slate-400 uppercase tracking-wider">Tags</span>
                        </div>
                        <div className="flex flex-wrap gap-2">
                            {tags.map(tag => (
                                <span
                                    key={tag}
                                    className="px-3 py-1.5 bg-slate-100 text-slate-600 rounded-full text-xs font-bold hover:bg-indigo-50 hover:text-indigo-600 transition-colors cursor-pointer"
                                >
                                    #{tag}
                                </span>
                            ))}
                        </div>
                    </div>
                )}

                {/* CTA */}
                <div className="mt-12 bg-gradient-to-br from-indigo-600 to-purple-700 rounded-3xl p-8 md:p-12 text-white text-center">
                    <h3 className="text-2xl font-black mb-3">Ce sujet vous concerne ?</h3>
                    <p className="text-white/70 font-medium mb-6 max-w-lg mx-auto">
                        Vérifiez votre éligibilité en quelques minutes avec notre simulateur intelligent et gratuit.
                    </p>
                    <Link
                        href="/"
                        className="inline-flex items-center gap-2 px-8 py-4 bg-white text-indigo-700 font-black rounded-2xl hover:bg-indigo-50 transition-all shadow-lg"
                    >
                        Tester mon éligibilité
                        <ArrowRight size={18} />
                    </Link>
                </div>
            </article>

            {/* ── Related Articles ── */}
            {relatedArticles.length > 0 && (
                <section className="max-w-6xl mx-auto px-6 py-16">
                    <h2 className="text-xl font-black text-slate-800 mb-6 uppercase tracking-wider flex items-center gap-2">
                        <BookOpen size={20} className="text-slate-400" />
                        Articles similaires
                    </h2>
                    <div className="grid md:grid-cols-3 gap-6">
                        {relatedArticles.map(a => (
                            <Link
                                key={a.id}
                                href={`/blog/${a.slug}`}
                                className="group bg-white rounded-2xl border border-slate-100 p-6 hover:shadow-lg transition-all duration-300 hover:-translate-y-1"
                            >
                                <span className={`inline-block px-2 py-0.5 rounded-md text-[10px] font-black uppercase tracking-wider mb-3 ${CATEGORY_COLORS[a.category] || 'bg-slate-100 text-slate-600'}`}>
                                    {CATEGORY_LABELS[a.category] || a.category}
                                </span>
                                <h3 className="font-bold text-slate-900 group-hover:text-indigo-600 transition-colors mb-2 line-clamp-2">
                                    {a.title}
                                </h3>
                                <p className="text-xs text-slate-400 font-medium line-clamp-2">{a.excerpt}</p>
                                <div className="flex items-center gap-2 mt-3 text-[11px] text-slate-400 font-bold">
                                    <Clock size={11} /> {a.readTimeMin} min
                                </div>
                            </Link>
                        ))}
                    </div>
                </section>
            )}

            {/* Footer */}
            <footer className="bg-slate-900 py-10 text-white/40 text-center text-xs font-bold uppercase tracking-widest">
                <p>© 2026 SimuLegal — Insights Juridiques</p>
            </footer>
        </div>
    );
}
