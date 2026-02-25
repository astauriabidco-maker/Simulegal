'use client';

import React, { useState, useEffect } from 'react';
import { useParams } from 'next/navigation';
import Link from 'next/link';
import {
    ArrowLeft, Clock, Eye, Calendar, Tag, User, Share2,
    BookOpen, Loader2, AlertCircle, ArrowRight, ChevronRight, MessageCircle, Send
} from 'lucide-react';
import { BlogStore, Article, ArticleComment } from '../../../services/BlogStore';

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

// ── Markdown → HTML renderer (enhanced) ──
function renderMarkdown(md: string): string {
    return md
        // Images: ![alt](url)
        .replace(/!\[([^\]]+)\]\(([^)]+)\)/g, '<img src="$2" alt="$1" class="w-full h-auto rounded-3xl my-8 shadow-sm border border-slate-100" />')
        // Links: [text](url)
        .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer" class="text-indigo-600 font-bold hover:underline">$1</a>')
        // Headers
        .replace(/^### (.+)$/gm, '<h3 class="text-xl font-black text-slate-900 mt-8 mb-3">$1</h3>')
        .replace(/^## (.+)$/gm, '<h2 class="text-2xl font-black text-slate-900 mt-10 mb-4">$1</h2>')
        // Bold
        .replace(/\*\*(.+?)\*\*/g, '<strong class="font-bold text-slate-900">$1</strong>')
        // Code blocks
        .replace(/```([\s\S]*?)```/g, '<pre class="bg-slate-900 text-slate-50 p-6 rounded-2xl my-6 overflow-x-auto text-sm font-mono leading-relaxed border border-slate-800">$1</pre>')
        // Inline code
        .replace(/`([^`]+)`/g, '<code class="bg-slate-100 text-pink-600 px-1.5 py-0.5 rounded-md text-sm font-mono mx-0.5 border border-slate-200">$1</code>')
        // Blockquote
        .replace(/^> (.+)$/gm, '<blockquote class="border-l-4 border-indigo-400 pl-5 py-3 my-6 bg-indigo-50/50 rounded-r-xl text-slate-600 italic font-medium">$1</blockquote>')
        // Unordered list items
        .replace(/^- (.+)$/gm, '<li class="flex items-start gap-2 py-1"><span class="w-1.5 h-1.5 rounded-full bg-indigo-500 mt-2 shrink-0"></span><span>$1</span></li>')
        // Ordered list items
        .replace(/^(\d+)\. (.+)$/gm, '<li class="flex items-start gap-3 py-1.5"><span class="w-6 h-6 rounded-full bg-indigo-100 text-indigo-700 flex items-center justify-center text-xs font-black shrink-0">$1</span><span>$2</span></li>')
        // Paragraphs (double newlines)
        .replace(/\n\n/g, '</p><p class="text-slate-600 leading-relaxed mb-4 font-medium">')
        // Single newlines
        .replace(/\n/g, '<br/>');
}

export default function BlogArticlePage() {
    const params = useParams();
    const slug = params.slug as string;

    const [article, setArticle] = useState<Article | null>(null);
    const [isLoading, setIsLoading] = useState(true);
    const [relatedArticles, setRelatedArticles] = useState<Article[]>([]);

    // Comments Form State
    const [commentName, setCommentName] = useState('');
    const [commentEmail, setCommentEmail] = useState('');
    const [commentContent, setCommentContent] = useState('');
    const [commentHoneypot, setCommentHoneypot] = useState(''); // honeypot
    const [isSubmittingComment, setIsSubmittingComment] = useState(false);
    const [commentFeedback, setCommentFeedback] = useState<{ type: 'success' | 'error', msg: string } | null>(null);

    useEffect(() => {
        const load = async () => {
            setIsLoading(true);
            const art = await BlogStore.getBySlug(slug);
            setArticle(art);

            if (art) {
                const { articles } = await BlogStore.getPublished(art.category, 4);
                setRelatedArticles(articles.filter(a => a.id !== art.id).slice(0, 3));
            }
            setIsLoading(false);
        };
        load();
    }, [slug]);

    const handleShare = () => {
        if (navigator.share) {
            navigator.share({ title: article?.title, url: window.location.href });
        } else {
            navigator.clipboard.writeText(window.location.href);
        }
    };

    const submitComment = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!commentContent.trim() || !commentName.trim()) return;
        setIsSubmittingComment(true);
        setCommentFeedback(null);
        try {
            const ok = await BlogStore.postComment(slug, {
                authorName: commentName,
                authorEmail: commentEmail,
                content: commentContent,
                honeypot: commentHoneypot
            });
            if (ok) {
                setCommentFeedback({ type: 'success', msg: 'Votre commentaire a été envoyé et est en attente de modération.' });
                setCommentContent('');
            } else {
                setCommentFeedback({ type: 'error', msg: 'Erreur lors de l\'envoi. Veuillez réessayer.' });
            }
        } catch (err) {
            setCommentFeedback({ type: 'error', msg: 'Erreur réseau.' });
        } finally {
            setIsSubmittingComment(false);
        }
    };

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

            {/* ── Cover Image ── */}
            {article.coverImage && (
                <div className="max-w-4xl mx-auto px-6 mb-12">
                    <img src={article.coverImage} alt={article.title} className="w-full h-auto max-h-[500px] object-cover rounded-3xl shadow-sm border border-slate-100" />
                </div>
            )}

            {/* ── Article Body ── */}
            <article className="max-w-4xl mx-auto px-6 pb-12">
                <div
                    className="prose prose-lg max-w-none text-slate-600 leading-relaxed font-medium"
                    dangerouslySetInnerHTML={{ __html: `<p class="text-slate-600 leading-relaxed mb-4 font-medium">${renderMarkdown(article.content)}</p>` }}
                />

                {/* Tags */}
                {tags.length > 0 && (
                    <div className="mt-12 pt-8 border-t border-slate-100">
                        <div className="flex flex-wrap gap-2">
                            {tags.map(tag => (
                                <span
                                    key={tag}
                                    className="px-3 py-1.5 bg-slate-50 text-slate-500 border border-slate-100 rounded-full text-xs font-bold hover:bg-slate-100 transition-colors cursor-pointer"
                                >
                                    #{tag}
                                </span>
                            ))}
                        </div>
                    </div>
                )}
            </article>

            {/* CTA */}
            <div className="max-w-4xl mx-auto px-6 pb-16">
                <div className="bg-gradient-to-br from-indigo-600 to-purple-700 rounded-3xl p-8 md:p-12 text-white text-center shadow-xl shadow-indigo-200">
                    <h3 className="text-2xl font-black mb-3">Ce sujet vous concerne ?</h3>
                    <p className="text-white/80 font-medium mb-6 max-w-lg mx-auto">
                        Vérifiez votre éligibilité en quelques minutes avec notre simulateur intelligent et gratuit.
                    </p>
                    <Link
                        href="/"
                        className="inline-flex items-center gap-2 px-8 py-4 bg-white text-indigo-700 font-black rounded-2xl hover:bg-slate-50 transition-all shadow-lg hover:shadow-xl hover:-translate-y-1"
                    >
                        Tester mon éligibilité
                        <ArrowRight size={18} />
                    </Link>
                </div>
            </div>

            {/* ── Comments Section ── */}
            <section className="bg-slate-50 border-t border-slate-100 py-16">
                <div className="max-w-4xl mx-auto px-6">
                    <div className="flex items-center gap-3 mb-8">
                        <MessageCircle className="text-indigo-500" size={28} />
                        <h2 className="text-2xl font-black text-slate-800">Commentaires ({article.comments?.length || 0})</h2>
                    </div>

                    {/* Comment List */}
                    <div className="space-y-6 mb-12">
                        {article.comments && article.comments.length > 0 ? (
                            article.comments.map(comment => (
                                <div key={comment.id} className="bg-white p-6 rounded-2xl border border-slate-100 shadow-sm">
                                    <div className="flex items-center justify-between mb-3">
                                        <div className="font-bold text-slate-800 flex items-center gap-2">
                                            <div className="w-8 h-8 rounded-full bg-slate-100 flex items-center justify-center text-slate-500 text-xs uppercase">
                                                {comment.authorName.charAt(0)}
                                            </div>
                                            {comment.authorName}
                                        </div>
                                        <span className="text-xs font-bold text-slate-400">
                                            {new Date(comment.createdAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'short', year: 'numeric' })}
                                        </span>
                                    </div>
                                    <p className="text-slate-600 font-medium leading-relaxed pl-10 border-l-2 border-transparent">
                                        {comment.content}
                                    </p>
                                </div>
                            ))
                        ) : (
                            <div className="text-center py-8 text-slate-400 font-medium bg-white rounded-2xl border border-slate-100 border-dashed">
                                Soyez le premier à commenter cet article.
                            </div>
                        )}
                    </div>

                    {/* Add Comment Form */}
                    <div className="bg-white p-8 rounded-3xl border border-slate-100 shadow-sm relative overflow-hidden">
                        <div className="absolute top-0 inset-x-0 h-1 bg-gradient-to-r from-indigo-500 to-purple-500"></div>
                        <h3 className="text-xl font-black text-slate-800 mb-6">Ajouter un commentaire</h3>

                        {commentFeedback && (
                            <div className={`p-4 rounded-xl mb-6 text-sm font-bold flex flex-col gap-1 ${commentFeedback.type === 'success' ? 'bg-emerald-50 text-emerald-700 border border-emerald-100' : 'bg-rose-50 text-rose-700 border border-rose-100'}`}>
                                {commentFeedback.msg}
                            </div>
                        )}

                        <form onSubmit={submitComment} className="space-y-5">
                            {/* Honeypot field - hidden from users */}
                            <input type="text" name="honeypot" className="hidden" tabIndex={-1} autoComplete="off" value={commentHoneypot} onChange={e => setCommentHoneypot(e.target.value)} />

                            <div className="grid md:grid-cols-2 gap-5">
                                <div>
                                    <label className="block text-xs font-black text-slate-500 uppercase tracking-wider mb-2">Nom *</label>
                                    <input
                                        type="text"
                                        required
                                        value={commentName}
                                        onChange={e => setCommentName(e.target.value)}
                                        className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-medium text-slate-800 placeholder-slate-400 focus:outline-none focus:ring-2 focus:ring-indigo-500/20 focus:border-indigo-500 transition-all"
                                        placeholder="Votre nom"
                                    />
                                </div>
                                <div>
                                    <label className="block text-xs font-black text-slate-500 uppercase tracking-wider mb-2">Email (ne sera pas publié)</label>
                                    <input
                                        type="email"
                                        value={commentEmail}
                                        onChange={e => setCommentEmail(e.target.value)}
                                        className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-medium text-slate-800 placeholder-slate-400 focus:outline-none focus:ring-2 focus:ring-indigo-500/20 focus:border-indigo-500 transition-all"
                                        placeholder="Votre adresse email"
                                    />
                                </div>
                            </div>
                            <div>
                                <label className="block text-xs font-black text-slate-500 uppercase tracking-wider mb-2">Commentaire *</label>
                                <textarea
                                    required
                                    rows={4}
                                    value={commentContent}
                                    onChange={e => setCommentContent(e.target.value)}
                                    className="w-full px-4 py-3 bg-slate-50 border border-slate-200 rounded-xl font-medium text-slate-800 placeholder-slate-400 focus:outline-none focus:ring-2 focus:ring-indigo-500/20 focus:border-indigo-500 transition-all resize-none"
                                    placeholder="Partagez votre avis..."
                                />
                            </div>
                            <div className="text-right">
                                <button
                                    type="submit"
                                    disabled={isSubmittingComment}
                                    className="inline-flex items-center gap-2 px-6 py-3 bg-slate-900 text-white font-bold rounded-xl hover:bg-slate-800 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
                                >
                                    {isSubmittingComment ? <Loader2 size={16} className="animate-spin" /> : <Send size={16} />}
                                    Publier le commentaire
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            </section>

            {/* ── Related Articles ── */}
            {relatedArticles.length > 0 && (
                <section className="max-w-6xl mx-auto px-6 py-16">
                    <h2 className="text-xl font-black text-slate-800 mb-8 uppercase tracking-wider flex items-center gap-2">
                        <BookOpen size={20} className="text-slate-400" />
                        Articles similaires
                    </h2>
                    <div className="grid md:grid-cols-3 gap-6">
                        {relatedArticles.map(a => (
                            <Link
                                key={a.id}
                                href={`/blog/${a.slug}`}
                                className="group bg-white rounded-3xl border border-slate-100 p-6 shadow-sm hover:shadow-xl hover:border-indigo-100 transition-all duration-300 hover:-translate-y-1 block"
                            >
                                <span className={`inline-block px-2.5 py-1 rounded-md text-[10px] font-black uppercase tracking-wider mb-4 ${CATEGORY_COLORS[a.category] || 'bg-slate-100 text-slate-600'}`}>
                                    {CATEGORY_LABELS[a.category] || a.category}
                                </span>
                                <h3 className="font-black text-slate-900 group-hover:text-indigo-600 transition-colors mb-2 line-clamp-2 text-lg">
                                    {a.title}
                                </h3>
                                <p className="text-sm text-slate-500 font-medium line-clamp-2">{a.excerpt}</p>
                                <div className="flex items-center gap-2 mt-5 pt-4 border-t border-slate-50 text-xs text-slate-400 font-bold">
                                    <Clock size={12} /> {a.readTimeMin} min de lecture
                                </div>
                            </Link>
                        ))}
                    </div>
                </section>
            )}

            {/* Footer */}
            <footer className="bg-slate-900 py-10 px-6 text-white/40 text-center text-xs font-bold uppercase tracking-widest flex flex-col items-center justify-center gap-4">
                <div className="flex items-center gap-6">
                    <a href="/public/blog/rss" target="_blank" rel="noopener noreferrer" className="hover:text-amber-500 transition-colors">RSS Feed</a>
                    <a href="/public/blog/sitemap.xml" target="_blank" rel="noopener noreferrer" className="hover:text-emerald-500 transition-colors">Sitemap</a>
                </div>
                <p>© 2026 SimuLegal — Insights Juridiques</p>
            </footer>
        </div>
    );
}
