'use client';

import React, { useState, useEffect, useRef, useCallback } from 'react';
import { useParams } from 'next/navigation';
import Link from 'next/link';
import {
    ArrowLeft, Clock, Eye, Calendar, Tag, User, Share2,
    BookOpen, Loader2, AlertCircle, ArrowRight, ChevronRight,
    MessageCircle, Send, Heart, Flame, ThumbsUp, Star,
    Twitter, Linkedin, Facebook, Link2, Check, Mail,
    ChevronDown, ChevronUp, List
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

const REACTION_CONFIG = [
    { type: 'LIKE', emoji: '👍', label: 'Utile' },
    { type: 'LOVE', emoji: '❤️', label: 'J\'aime' },
    { type: 'FIRE', emoji: '🔥', label: 'Top' },
    { type: 'USEFUL', emoji: '⭐', label: 'Favori' },
];

// ── Sanitize HTML to prevent XSS ──
function sanitizeHtml(html: string): string {
    return html
        .replace(/<script[\s\S]*?<\/script>/gi, '')
        .replace(/on\w+="[^"]*"/gi, '')
        .replace(/on\w+='[^']*'/gi, '')
        .replace(/javascript:/gi, '');
}

// ── Extract headings for Table of Contents ──
function extractHeadings(md: string): { level: number; text: string; id: string }[] {
    const headings: { level: number; text: string; id: string }[] = [];
    const regex = /^(#{2,3})\s+(.+)$/gm;
    let match;
    while ((match = regex.exec(md)) !== null) {
        const level = match[1].length;
        const text = match[2].trim();
        const id = text.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '')
            .replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
        headings.push({ level, text, id });
    }
    return headings;
}

// ── Markdown → HTML renderer (enhanced with IDs for TOC + syntax highlight) ──
function renderMarkdown(md: string): string {
    let html = md
        .replace(/!\[([^\]]+)\]\(([^)]+)\)/g, '<img src="$2" alt="$1" loading="lazy" class="w-full h-auto rounded-3xl my-8 shadow-sm border border-slate-100" />')
        .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer" class="text-indigo-600 font-bold hover:underline">$1</a>')
        .replace(/^### (.+)$/gm, (_, text) => {
            const id = text.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
            return `<h3 id="${id}" class="text-xl font-black text-slate-900 mt-8 mb-3 scroll-mt-24">${text}</h3>`;
        })
        .replace(/^## (.+)$/gm, (_, text) => {
            const id = text.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
            return `<h2 id="${id}" class="text-2xl font-black text-slate-900 mt-10 mb-4 scroll-mt-24">${text}</h2>`;
        })
        .replace(/\*\*(.+?)\*\*/g, '<strong class="font-bold text-slate-900">$1</strong>')
        .replace(/```(\w+)?\n([\s\S]*?)```/g, '<pre class="bg-slate-900 text-slate-50 p-6 rounded-2xl my-6 overflow-x-auto text-sm font-mono leading-relaxed border border-slate-800"><code>$2</code></pre>')
        .replace(/`([^`]+)`/g, '<code class="bg-slate-100 text-pink-600 px-1.5 py-0.5 rounded-md text-sm font-mono mx-0.5 border border-slate-200">$1</code>')
        .replace(/^> (.+)$/gm, '<blockquote class="border-l-4 border-indigo-400 pl-5 py-3 my-6 bg-indigo-50/50 rounded-r-xl text-slate-600 italic font-medium">$1</blockquote>')
        .replace(/^- (.+)$/gm, '<li class="flex items-start gap-2 py-1"><span class="w-1.5 h-1.5 rounded-full bg-indigo-500 mt-2 shrink-0"></span><span>$1</span></li>')
        .replace(/^(\d+)\. (.+)$/gm, '<li class="flex items-start gap-3 py-1.5"><span class="w-6 h-6 rounded-full bg-indigo-100 text-indigo-700 flex items-center justify-center text-xs font-black shrink-0">$1</span><span>$2</span></li>')
        .replace(/\n\n/g, '</p><p class="text-slate-600 leading-relaxed mb-4 font-medium">')
        .replace(/\n/g, '<br/>');
    return sanitizeHtml(html);
}

export default function BlogArticlePage() {
    const params = useParams();
    const slug = params.slug as string;

    const [article, setArticle] = useState<Article | null>(null);
    const [related, setRelated] = useState<Article[]>([]);
    const [isLoading, setIsLoading] = useState(true);

    // Comments
    const [commentName, setCommentName] = useState('');
    const [commentEmail, setCommentEmail] = useState('');
    const [commentContent, setCommentContent] = useState('');
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [feedback, setFeedback] = useState('');
    const [honeypot, setHoneypot] = useState('');
    const [replyingTo, setReplyingTo] = useState<string | null>(null);
    const [replyContent, setReplyContent] = useState('');
    const [replyName, setReplyName] = useState('');

    // Reactions
    const [reactions, setReactions] = useState<Record<string, number>>({});
    const [reactedTypes, setReactedTypes] = useState<Set<string>>(new Set());

    // Reading progress
    const [progress, setProgress] = useState(0);
    const contentRef = useRef<HTMLDivElement>(null);

    // TOC
    const [showToc, setShowToc] = useState(true);
    const [activeHeading, setActiveHeading] = useState('');

    // Share
    const [copied, setCopied] = useState(false);

    // Newsletter
    const [nlEmail, setNlEmail] = useState('');
    const [nlStatus, setNlStatus] = useState<'idle' | 'loading' | 'success' | 'error'>('idle');

    useEffect(() => {
        if (!slug) return;
        (async () => {
            setIsLoading(true);
            const data = await BlogStore.getBySlug(slug);
            setArticle(data);
            if (data) {
                setReactions(data.reactionCounts || {});
                const relData = await BlogStore.getPublished(data.category, 4);
                setRelated(relData.articles.filter(a => a.slug !== slug).slice(0, 3));
            }
            setIsLoading(false);
        })();
    }, [slug]);

    // Reading progress bar
    useEffect(() => {
        const handleScroll = () => {
            if (!contentRef.current) return;
            const rect = contentRef.current.getBoundingClientRect();
            const total = contentRef.current.scrollHeight - window.innerHeight;
            const scrolled = window.scrollY - contentRef.current.offsetTop;
            setProgress(Math.max(0, Math.min(100, (scrolled / total) * 100)));

            // Scroll spy for TOC
            if (article) {
                const headings = extractHeadings(article.content);
                for (let i = headings.length - 1; i >= 0; i--) {
                    const el = document.getElementById(headings[i].id);
                    if (el && el.getBoundingClientRect().top <= 120) {
                        setActiveHeading(headings[i].id);
                        break;
                    }
                }
            }
        };
        window.addEventListener('scroll', handleScroll, { passive: true });
        return () => window.removeEventListener('scroll', handleScroll);
    }, [article]);

    const headings = article ? extractHeadings(article.content) : [];

    const submitComment = async (parentId?: string) => {
        const name = parentId ? replyName : commentName;
        const content = parentId ? replyContent : commentContent;
        if (!name.trim() || !content.trim() || honeypot) return;
        setIsSubmitting(true);
        const ok = await BlogStore.postComment(slug, {
            authorName: name.trim(),
            authorEmail: parentId ? undefined : commentEmail,
            content: content.trim(),
            parentId,
            honeypot,
        });
        setIsSubmitting(false);
        if (ok) {
            setFeedback('✅ Commentaire soumis ! Il sera visible après modération.');
            if (parentId) { setReplyContent(''); setReplyName(''); setReplyingTo(null); }
            else { setCommentContent(''); }
        } else {
            setFeedback('❌ Erreur lors de l\'envoi. Réessayez.');
        }
        setTimeout(() => setFeedback(''), 5000);
    };

    const handleReaction = async (type: string) => {
        if (!article) return;
        const result = await BlogStore.toggleReaction(article.id, type);
        if (result) {
            setReactions(prev => ({
                ...prev,
                [type]: (prev[type] || 0) + (result.toggled === 'added' ? 1 : -1),
            }));
            setReactedTypes(prev => {
                const next = new Set(prev);
                if (result.toggled === 'added') { next.add(type); } else { next.delete(type); }
                return next;
            });
        }
    };

    const shareUrl = typeof window !== 'undefined' ? window.location.href : '';
    const shareTitle = article?.title || '';

    const copyLink = () => {
        navigator.clipboard.writeText(shareUrl);
        setCopied(true);
        setTimeout(() => setCopied(false), 2000);
    };

    const subscribeNewsletter = async () => {
        if (!nlEmail.trim()) return;
        setNlStatus('loading');
        const result = await BlogStore.subscribe(nlEmail);
        setNlStatus(result.success ? 'success' : 'error');
        if (result.success) setNlEmail('');
    };

    if (isLoading) {
        return (
            <div className="min-h-screen bg-white flex items-center justify-center">
                <Loader2 className="w-8 h-8 text-indigo-600 animate-spin" />
            </div>
        );
    }

    if (!article) {
        return (
            <div className="min-h-screen bg-white flex flex-col items-center justify-center gap-4">
                <AlertCircle className="w-12 h-12 text-red-400" />
                <h1 className="text-2xl font-bold text-slate-900">Article non trouvé</h1>
                <Link href="/blog" className="text-indigo-600 hover:underline flex items-center gap-2">
                    <ArrowLeft className="w-4 h-4" /> Retour au blog
                </Link>
            </div>
        );
    }

    const comments = (article as any).comments || [];
    const rootComments = comments.filter((c: any) => !c.parentId);
    const getReplies = (parentId: string) => comments.filter((c: any) => c.parentId === parentId);

    return (
        <>
            {/* ── Reading Progress Bar ── */}
            <div className="fixed top-0 left-0 right-0 z-50 h-1 bg-slate-100">
                <div
                    className="h-full bg-gradient-to-r from-indigo-500 via-purple-500 to-pink-500 transition-all duration-150"
                    style={{ width: `${progress}%` }}
                />
            </div>

            <div className="min-h-screen bg-white" ref={contentRef}>
                {/* ── Header / Hero ── */}
                <div className="bg-gradient-to-br from-slate-900 via-indigo-950 to-slate-900 text-white">
                    <div className="max-w-4xl mx-auto px-6 py-16">
                        <Link href="/blog" className="inline-flex items-center gap-2 text-indigo-300 hover:text-white mb-8 text-sm font-medium transition-colors">
                            <ArrowLeft className="w-4 h-4" /> Retour aux articles
                        </Link>

                        <div className="flex flex-wrap items-center gap-3 mb-6">
                            <span className={`px-3 py-1 rounded-full text-xs font-bold ${CATEGORY_COLORS[article.category] || CATEGORY_COLORS.GENERAL}`}>
                                {CATEGORY_LABELS[article.category] || article.category}
                            </span>
                            {article.tags?.split(',').map(tag => (
                                <Link key={tag.trim()} href={`/blog?tag=${tag.trim()}`}
                                    className="px-2 py-0.5 bg-white/10 rounded-full text-xs text-indigo-200 hover:bg-white/20 transition-colors">
                                    #{tag.trim()}
                                </Link>
                            ))}
                        </div>

                        <h1 className="text-4xl md:text-5xl font-black leading-tight mb-6">{article.title}</h1>

                        {article.excerpt && (
                            <p className="text-lg text-indigo-200 leading-relaxed mb-8 max-w-3xl">{article.excerpt}</p>
                        )}

                        <div className="flex flex-wrap items-center gap-6 text-sm text-indigo-300">
                            <span className="flex items-center gap-2"><User className="w-4 h-4" />{article.authorName}</span>
                            <span className="flex items-center gap-2"><Calendar className="w-4 h-4" />{article.publishedAt ? new Date(article.publishedAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' }) : ''}</span>
                            <span className="flex items-center gap-2"><Clock className="w-4 h-4" />{article.readTimeMin} min de lecture</span>
                            <span className="flex items-center gap-2"><Eye className="w-4 h-4" />{article.viewCount} vues</span>
                            <span className="flex items-center gap-2"><MessageCircle className="w-4 h-4" />{comments.length} commentaire{comments.length !== 1 ? 's' : ''}</span>
                        </div>
                    </div>
                </div>

                {/* ── Content Area ── */}
                <div className="max-w-6xl mx-auto px-6 py-12 flex gap-12">
                    {/* ── Table of Contents (sidebar) ── */}
                    {headings.length > 3 && (
                        <aside className="hidden lg:block w-64 shrink-0">
                            <div className="sticky top-20">
                                <button onClick={() => setShowToc(!showToc)}
                                    className="flex items-center gap-2 text-sm font-bold text-slate-700 mb-4 hover:text-indigo-600 transition-colors">
                                    <List className="w-4 h-4" />
                                    Sommaire
                                    {showToc ? <ChevronUp className="w-3 h-3" /> : <ChevronDown className="w-3 h-3" />}
                                </button>
                                {showToc && (
                                    <nav className="space-y-1 border-l-2 border-slate-200">
                                        {headings.map(h => (
                                            <a key={h.id} href={`#${h.id}`}
                                                className={`block text-sm py-1 transition-colors ${h.level === 3 ? 'pl-6' : 'pl-4'} ${activeHeading === h.id
                                                    ? 'text-indigo-600 font-bold border-l-2 border-indigo-600 -ml-[2px]'
                                                    : 'text-slate-500 hover:text-slate-700'
                                                    }`}>
                                                {h.text}
                                            </a>
                                        ))}
                                    </nav>
                                )}
                            </div>
                        </aside>
                    )}

                    {/* ── Article Content ── */}
                    <article className="flex-1 min-w-0">
                        {article.coverImage && (
                            <img src={article.coverImage} alt={article.title}
                                loading="lazy"
                                className="w-full h-80 object-cover rounded-3xl mb-10 shadow-lg" />
                        )}

                        <div className="prose prose-lg max-w-none"
                            dangerouslySetInnerHTML={{ __html: `<p class="text-slate-600 leading-relaxed mb-4 font-medium">${renderMarkdown(article.content)}</p>` }}
                        />

                        {/* ── Reactions Bar ── */}
                        <div className="mt-12 pt-8 border-t border-slate-200">
                            <p className="text-sm font-bold text-slate-700 mb-4">Cet article vous a été utile ?</p>
                            <div className="flex flex-wrap gap-3">
                                {REACTION_CONFIG.map(r => {
                                    const count = reactions[r.type] || 0;
                                    const isActive = reactedTypes.has(r.type);
                                    return (
                                        <button key={r.type} onClick={() => handleReaction(r.type)}
                                            className={`flex items-center gap-2 px-5 py-2.5 rounded-full text-sm font-bold transition-all duration-200
                                            ${isActive
                                                    ? 'bg-indigo-100 text-indigo-700 ring-2 ring-indigo-300 scale-105'
                                                    : 'bg-slate-100 text-slate-600 hover:bg-slate-200 hover:scale-105'
                                                }`}>
                                            <span className="text-lg">{r.emoji}</span>
                                            {r.label}
                                            {count > 0 && <span className="bg-white/80 px-2 py-0.5 rounded-full text-xs">{count}</span>}
                                        </button>
                                    );
                                })}
                            </div>
                        </div>

                        {/* ── Social Sharing ── */}
                        <div className="mt-8 pt-6 border-t border-slate-100">
                            <p className="text-sm font-bold text-slate-700 mb-4">Partager cet article</p>
                            <div className="flex flex-wrap gap-3">
                                <a href={`https://twitter.com/intent/tweet?text=${encodeURIComponent(shareTitle)}&url=${encodeURIComponent(shareUrl)}`}
                                    target="_blank" rel="noopener noreferrer"
                                    className="flex items-center gap-2 px-4 py-2 bg-sky-50 text-sky-600 rounded-full text-sm font-bold hover:bg-sky-100 transition-colors">
                                    <Twitter className="w-4 h-4" /> Twitter
                                </a>
                                <a href={`https://www.linkedin.com/sharing/share-offsite/?url=${encodeURIComponent(shareUrl)}`}
                                    target="_blank" rel="noopener noreferrer"
                                    className="flex items-center gap-2 px-4 py-2 bg-blue-50 text-blue-600 rounded-full text-sm font-bold hover:bg-blue-100 transition-colors">
                                    <Linkedin className="w-4 h-4" /> LinkedIn
                                </a>
                                <a href={`https://www.facebook.com/sharer/sharer.php?u=${encodeURIComponent(shareUrl)}`}
                                    target="_blank" rel="noopener noreferrer"
                                    className="flex items-center gap-2 px-4 py-2 bg-indigo-50 text-indigo-600 rounded-full text-sm font-bold hover:bg-indigo-100 transition-colors">
                                    <Facebook className="w-4 h-4" /> Facebook
                                </a>
                                <button onClick={copyLink}
                                    className="flex items-center gap-2 px-4 py-2 bg-slate-100 text-slate-600 rounded-full text-sm font-bold hover:bg-slate-200 transition-colors">
                                    {copied ? <Check className="w-4 h-4 text-green-600" /> : <Link2 className="w-4 h-4" />}
                                    {copied ? 'Copié !' : 'Copier le lien'}
                                </button>
                            </div>
                        </div>

                        {/* ── Newsletter CTA ── */}
                        <div className="mt-10 p-8 bg-gradient-to-br from-indigo-50 to-purple-50 rounded-3xl border border-indigo-100">
                            <div className="flex items-start gap-4">
                                <div className="w-12 h-12 bg-indigo-100 rounded-2xl flex items-center justify-center shrink-0">
                                    <Mail className="w-6 h-6 text-indigo-600" />
                                </div>
                                <div className="flex-1">
                                    <h3 className="text-lg font-bold text-slate-900 mb-1">Restez informé</h3>
                                    <p className="text-sm text-slate-600 mb-4">Recevez nos derniers articles juridiques directement dans votre boîte mail.</p>
                                    {nlStatus === 'success' ? (
                                        <p className="text-green-600 font-bold text-sm flex items-center gap-2"><Check className="w-4 h-4" /> Inscription réussie !</p>
                                    ) : (
                                        <div className="flex gap-3">
                                            <input type="email" value={nlEmail} onChange={e => setNlEmail(e.target.value)}
                                                placeholder="votre@email.com"
                                                className="flex-1 px-4 py-2.5 rounded-xl border border-slate-200 text-sm focus:ring-2 focus:ring-indigo-300 focus:border-indigo-400 outline-none" />
                                            <button onClick={subscribeNewsletter} disabled={nlStatus === 'loading'}
                                                className="px-6 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-colors disabled:opacity-50">
                                                {nlStatus === 'loading' ? 'Envoi...' : 'S\'inscrire'}
                                            </button>
                                        </div>
                                    )}
                                </div>
                            </div>
                        </div>

                        {/* ── Comments Section ── */}
                        <div className="mt-12 pt-8 border-t border-slate-200">
                            <h2 className="text-2xl font-black text-slate-900 mb-8 flex items-center gap-3">
                                <MessageCircle className="w-6 h-6 text-indigo-500" />
                                Commentaires ({comments.length})
                            </h2>

                            {/* Comment Form */}
                            <div className="bg-slate-50 rounded-2xl p-6 mb-8 border border-slate-200">
                                <h3 className="font-bold text-slate-800 mb-4">Laisser un commentaire</h3>
                                <div className="grid grid-cols-2 gap-4 mb-4">
                                    <input type="text" value={commentName} onChange={e => setCommentName(e.target.value)}
                                        placeholder="Votre nom *" className="px-4 py-2.5 rounded-xl border border-slate-200 text-sm focus:ring-2 focus:ring-indigo-300 outline-none" />
                                    <input type="email" value={commentEmail} onChange={e => setCommentEmail(e.target.value)}
                                        placeholder="Votre email (optionnel)" className="px-4 py-2.5 rounded-xl border border-slate-200 text-sm focus:ring-2 focus:ring-indigo-300 outline-none" />
                                </div>
                                <input type="text" value={honeypot} onChange={e => setHoneypot(e.target.value)} className="hidden" tabIndex={-1} autoComplete="off" />
                                <textarea value={commentContent} onChange={e => setCommentContent(e.target.value)} rows={4}
                                    placeholder="Votre commentaire..." className="w-full px-4 py-3 rounded-xl border border-slate-200 text-sm focus:ring-2 focus:ring-indigo-300 outline-none resize-none mb-4" />
                                <div className="flex items-center justify-between">
                                    <span className="text-xs text-slate-400">Les commentaires sont modérés avant publication.</span>
                                    <button onClick={() => submitComment()} disabled={isSubmitting || !commentName.trim() || !commentContent.trim()}
                                        className="flex items-center gap-2 px-6 py-2.5 bg-indigo-600 text-white rounded-xl text-sm font-bold hover:bg-indigo-700 transition-all disabled:opacity-50">
                                        {isSubmitting ? <Loader2 className="w-4 h-4 animate-spin" /> : <Send className="w-4 h-4" />}
                                        Publier
                                    </button>
                                </div>
                                {feedback && <p className="mt-3 text-sm font-medium">{feedback}</p>}
                            </div>

                            {/* Comments List - Threaded */}
                            <div className="space-y-6">
                                {rootComments.map((c: any) => (
                                    <div key={c.id}>
                                        <div className="flex gap-4">
                                            <div className="w-10 h-10 rounded-full bg-gradient-to-br from-indigo-400 to-purple-500 flex items-center justify-center text-white font-bold text-sm shrink-0">
                                                {c.authorName.charAt(0).toUpperCase()}
                                            </div>
                                            <div className="flex-1">
                                                <div className="flex items-center gap-3 mb-1">
                                                    <span className="font-bold text-slate-900 text-sm">{c.authorName}</span>
                                                    <span className="text-xs text-slate-400">{new Date(c.createdAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' })}</span>
                                                </div>
                                                <p className="text-slate-600 text-sm leading-relaxed">{c.content}</p>
                                                <button onClick={() => { setReplyingTo(replyingTo === c.id ? null : c.id); setReplyContent(''); }}
                                                    className="mt-2 text-xs font-bold text-indigo-600 hover:text-indigo-800">
                                                    {replyingTo === c.id ? 'Annuler' : 'Répondre'}
                                                </button>

                                                {replyingTo === c.id && (
                                                    <div className="mt-3 pl-4 border-l-2 border-indigo-200">
                                                        <input type="text" value={replyName} onChange={e => setReplyName(e.target.value)}
                                                            placeholder="Votre nom *" className="w-full px-3 py-2 rounded-lg border border-slate-200 text-sm mb-2 outline-none focus:ring-2 focus:ring-indigo-300" />
                                                        <textarea value={replyContent} onChange={e => setReplyContent(e.target.value)} rows={2}
                                                            placeholder="Votre réponse..." className="w-full px-3 py-2 rounded-lg border border-slate-200 text-sm mb-2 outline-none resize-none focus:ring-2 focus:ring-indigo-300" />
                                                        <button onClick={() => submitComment(c.id)} disabled={isSubmitting || !replyName.trim() || !replyContent.trim()}
                                                            className="px-4 py-1.5 bg-indigo-600 text-white rounded-lg text-xs font-bold hover:bg-indigo-700 disabled:opacity-50">
                                                            {isSubmitting ? 'Envoi...' : 'Répondre'}
                                                        </button>
                                                    </div>
                                                )}
                                            </div>
                                        </div>

                                        {/* Replies */}
                                        {getReplies(c.id).map((reply: any) => (
                                            <div key={reply.id} className="ml-14 mt-4 flex gap-3">
                                                <div className="w-8 h-8 rounded-full bg-slate-200 flex items-center justify-center text-slate-500 font-bold text-xs shrink-0">
                                                    {reply.authorName.charAt(0).toUpperCase()}
                                                </div>
                                                <div>
                                                    <div className="flex items-center gap-2 mb-1">
                                                        <span className="font-bold text-slate-800 text-xs">{reply.authorName}</span>
                                                        <span className="text-xs text-slate-400">{new Date(reply.createdAt).toLocaleDateString('fr-FR')}</span>
                                                    </div>
                                                    <p className="text-slate-600 text-sm">{reply.content}</p>
                                                </div>
                                            </div>
                                        ))}
                                    </div>
                                ))}

                                {comments.length === 0 && (
                                    <p className="text-center text-slate-400 py-8">Aucun commentaire. Soyez le premier !</p>
                                )}
                            </div>
                        </div>

                        {/* ── Related Articles ── */}
                        {related.length > 0 && (
                            <div className="mt-16 pt-8 border-t border-slate-200">
                                <h2 className="text-2xl font-black text-slate-900 mb-8">Articles similaires</h2>
                                <div className="grid md:grid-cols-3 gap-6">
                                    {related.map(r => (
                                        <Link key={r.id} href={`/blog/${r.slug}`}
                                            className="group bg-white rounded-2xl border border-slate-200 overflow-hidden hover:shadow-lg hover:-translate-y-1 transition-all duration-300">
                                            {r.coverImage && (
                                                <img src={r.coverImage} alt={r.title} loading="lazy" className="w-full h-36 object-cover" />
                                            )}
                                            <div className="p-5">
                                                <span className={`px-2 py-0.5 rounded-full text-xs font-bold ${CATEGORY_COLORS[r.category] || CATEGORY_COLORS.GENERAL}`}>
                                                    {CATEGORY_LABELS[r.category] || r.category}
                                                </span>
                                                <h3 className="font-bold text-slate-900 mt-3 text-sm leading-tight group-hover:text-indigo-600 transition-colors">{r.title}</h3>
                                                <p className="text-xs text-slate-500 mt-2 line-clamp-2">{r.excerpt}</p>
                                            </div>
                                        </Link>
                                    ))}
                                </div>
                            </div>
                        )}
                    </article>
                </div>

                {/* ── JSON-LD Schema.org ── */}
                <script type="application/ld+json" dangerouslySetInnerHTML={{
                    __html: JSON.stringify({
                        '@context': 'https://schema.org',
                        '@type': 'BlogPosting',
                        headline: article.title,
                        description: article.excerpt || article.metaDescription,
                        author: { '@type': 'Person', name: article.authorName },
                        datePublished: article.publishedAt,
                        dateModified: article.updatedAt,
                        image: article.coverImage || undefined,
                        publisher: { '@type': 'Organization', name: 'SimuLegal', url: 'https://simulegal.fr' },
                        mainEntityOfPage: { '@type': 'WebPage', '@id': shareUrl },
                        wordCount: article.content.split(/\s+/).length,
                        commentCount: comments.length,
                    }),
                }} />
            </div>
        </>
    );
}
