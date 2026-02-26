'use client';

import React, { useState, useEffect, useRef, useCallback } from 'react';
import {
    Plus, Edit3, Trash2, Eye, EyeOff, Star, Search,
    FileText, Calendar, BarChart3, ArrowLeft, Save,
    Globe, Tag, Clock, ChevronDown, Loader2, X, StarOff,
    MessageCircle, CheckCircle, XCircle, Zap, Settings, Play, RefreshCw,
    Bot, Sparkles, TrendingUp, AlertTriangle, ExternalLink, RotateCcw,
    Bold, Heading2, Heading3, List, ListOrdered, Quote, Code, Link, Image, Upload
} from 'lucide-react';
import { useSearchParams } from 'next/navigation';
import { BlogStore, Article, ArticleCategory, ArticleStatus, ArticleComment, BlogAutoTopic, BlogAutoConfig, BlogAutoStats } from '../../../services/BlogStore';

const CATEGORIES: { id: ArticleCategory | 'ALL'; label: string; icon: string }[] = [
    { id: 'ALL', label: 'Tous', icon: '📋' },
    { id: 'GENERAL', label: 'Général', icon: '📰' },
    { id: 'IMMIGRATION', label: 'Immigration', icon: '🌍' },
    { id: 'NATURALISATION', label: 'Naturalisation', icon: '🇫🇷' },
    { id: 'SEJOUR', label: 'Titre de séjour', icon: '📄' },
    { id: 'PERMIS', label: 'Permis de conduire', icon: '🚗' },
    { id: 'FAMILY', label: 'Famille', icon: '👨‍👩‍👧‍👦' },
];

const STATUS_BADGES: Record<ArticleStatus, { label: string; cls: string }> = {
    DRAFT: { label: 'Brouillon', cls: 'bg-amber-100 text-amber-700' },
    PUBLISHED: { label: 'Publié', cls: 'bg-emerald-100 text-emerald-700' },
    ARCHIVED: { label: 'Archivé', cls: 'bg-slate-100 text-slate-500' },
    SCHEDULED: { label: 'Planifié', cls: 'bg-purple-100 text-purple-700' },
};

const STAT_COLORS: Record<string, { bg: string; text: string }> = {
    indigo: { bg: 'bg-indigo-50', text: 'text-indigo-500' },
    emerald: { bg: 'bg-emerald-50', text: 'text-emerald-500' },
    blue: { bg: 'bg-blue-50', text: 'text-blue-500' },
    amber: { bg: 'bg-amber-50', text: 'text-amber-500' },
};

function renderMarkdownPreview(md: string): string {
    if (!md) return '';
    return md
        .replace(/!\[([^\]]*)\]\(([^)]+)\)/g, '<img src="$2" alt="$1" style="max-width:100%; border-radius:12px; margin: 12px 0; box-shadow: 0 2px 8px rgba(0,0,0,0.08);" />')
        .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" style="color: #4f46e5; font-weight: 700; text-decoration: underline;">$1</a>')
        .replace(/^### (.+)$/gm, '<h3 style="font-size: 1.25rem; font-weight: 800; margin-top: 1.5rem; margin-bottom: 0.5rem; color: #0f172a;">$1</h3>')
        .replace(/^## (.+)$/gm, '<h2 style="font-size: 1.5rem; font-weight: 800; margin-top: 2rem; margin-bottom: 0.75rem; color: #0f172a;">$1</h2>')
        .replace(/\*\*(.+?)\*\*/g, '<strong style="font-weight: 700; color: #0f172a;">$1</strong>')
        .replace(/```([\s\S]*?)```/g, '<pre style="background: #1e293b; color: #f8fafc; padding: 1rem; border-radius: 12px; overflow-x: auto; font-family: monospace; font-size: 0.875rem; margin: 1rem 0;">$1</pre>')
        .replace(/`([^`]+)`/g, '<code style="background: #f1f5f9; color: #db2777; padding: 2px 6px; border-radius: 4px; font-family: monospace; font-size: 0.875rem;">$1</code>')
        .replace(/^> (.+)$/gm, '<blockquote style="border-left: 4px solid #818cf8; padding: 0.75rem 1rem; margin: 1rem 0; background: #eef2ff; border-radius: 0 8px 8px 0; font-style: italic; color: #475569;">$1</blockquote>')
        .replace(/^- (.+)$/gm, '<li style="margin-left: 1.5rem; list-style-type: disc; margin-bottom: 0.25rem;">$1</li>')
        .replace(/^(\d+)\. (.+)$/gm, '<li style="margin-left: 1.5rem; list-style-type: decimal; margin-bottom: 0.25rem;">$2</li>')
        .replace(/\n\n/g, '</p><p style="margin-bottom: 1rem; line-height: 1.7; color: #475569;">')
        .replace(/\n/g, '<br/>');
}

// ── Markdown Toolbar Component ──
function MarkdownToolbar({
    textareaRef,
    content,
    onChange,
    onUploadImage,
}: {
    textareaRef: React.RefObject<HTMLTextAreaElement | null>;
    content: string;
    onChange: (val: string) => void;
    onUploadImage: () => void;
}) {
    const wrap = (before: string, after: string) => {
        const ta = textareaRef.current;
        if (!ta) return;
        const start = ta.selectionStart;
        const end = ta.selectionEnd;
        const selected = content.substring(start, end);
        const replacement = before + (selected || 'texte') + after;
        const newContent = content.substring(0, start) + replacement + content.substring(end);
        onChange(newContent);
        setTimeout(() => {
            ta.focus();
            const newPos = start + before.length;
            ta.setSelectionRange(newPos, newPos + (selected || 'texte').length);
        }, 10);
    };

    const insertLine = (prefix: string) => {
        const ta = textareaRef.current;
        if (!ta) return;
        const start = ta.selectionStart;
        const lineStart = content.lastIndexOf('\n', start - 1) + 1;
        const newContent = content.substring(0, lineStart) + prefix + content.substring(lineStart);
        onChange(newContent);
        setTimeout(() => { ta.focus(); ta.setSelectionRange(start + prefix.length, start + prefix.length); }, 10);
    };

    const insertAtCursor = (text: string) => {
        const ta = textareaRef.current;
        if (!ta) return;
        const start = ta.selectionStart;
        const newContent = content.substring(0, start) + text + content.substring(start);
        onChange(newContent);
        setTimeout(() => { ta.focus(); ta.setSelectionRange(start + text.length, start + text.length); }, 10);
    };

    const tools = [
        { icon: Bold, title: 'Gras (Ctrl+B)', action: () => wrap('**', '**') },
        { icon: Heading2, title: 'Titre H2', action: () => insertLine('## ') },
        { icon: Heading3, title: 'Titre H3', action: () => insertLine('### ') },
        null, // separator
        { icon: List, title: 'Liste à puces', action: () => insertLine('- ') },
        { icon: ListOrdered, title: 'Liste numérotée', action: () => insertLine('1. ') },
        { icon: Quote, title: 'Citation', action: () => insertLine('> ') },
        null,
        { icon: Code, title: 'Code inline', action: () => wrap('`', '`') },
        { icon: Link, title: 'Lien', action: () => insertAtCursor('[texte du lien](https://url)') },
        { icon: Image, title: 'Image (URL)', action: () => insertAtCursor('![description](https://url-image)') },
        null,
        { icon: Upload, title: 'Uploader une image', action: onUploadImage },
    ];

    return (
        <div className="flex items-center gap-0.5 bg-slate-100 rounded-xl p-1.5 flex-wrap">
            {tools.map((tool, i) =>
                tool === null ? (
                    <div key={`sep-${i}`} className="w-px h-6 bg-slate-300 mx-1" />
                ) : (
                    <button
                        key={tool.title}
                        type="button"
                        onClick={tool.action}
                        title={tool.title}
                        className="p-1.5 rounded-lg text-slate-500 hover:bg-white hover:text-indigo-600 hover:shadow-sm transition-all"
                    >
                        <tool.icon size={16} />
                    </button>
                )
            )}
        </div>
    );
}

export default function BlogAdminPage() {
    const searchParams = useSearchParams();
    const initialTab = searchParams.get('tab')?.toUpperCase() as any;

    const [activeTab, setActiveTab] = useState<'ARTICLES' | 'COMMENTS' | 'VEILLE'>(
        ['ARTICLES', 'COMMENTS', 'VEILLE'].includes(initialTab) ? initialTab : 'ARTICLES'
    );
    const [articles, setArticles] = useState<Article[]>([]);
    const [isLoading, setIsLoading] = useState(true);
    const [filterStatus, setFilterStatus] = useState<string>('ALL');
    const [searchQuery, setSearchQuery] = useState('');
    const [pendingComments, setPendingComments] = useState<ArticleComment[]>([]);
    const [isLoadingComments, setIsLoadingComments] = useState(false);

    useEffect(() => {
        const tab = searchParams.get('tab')?.toUpperCase() as any;
        if (['ARTICLES', 'COMMENTS', 'VEILLE'].includes(tab)) {
            setActiveTab(tab);
        }
    }, [searchParams]);

    // ── Veille Auto state ──
    const [autoTopics, setAutoTopics] = useState<BlogAutoTopic[]>([]);
    const [autoStats, setAutoStats] = useState<BlogAutoStats | null>(null);
    const [autoConfig, setAutoConfig] = useState<BlogAutoConfig | null>(null);
    const [isLoadingVeille, setIsLoadingVeille] = useState(false);
    const [isPipelineRunning, setIsPipelineRunning] = useState(false);
    const [pipelineResult, setPipelineResult] = useState<string | null>(null);
    const [showConfigPanel, setShowConfigPanel] = useState(false);
    const [topicFilter, setTopicFilter] = useState<string>('ALL');

    // Editor state
    const [showEditor, setShowEditor] = useState(false);
    const [editingArticle, setEditingArticle] = useState<Article | null>(null);
    const [form, setForm] = useState({
        title: '', slug: '', excerpt: '', content: '',
        category: 'GENERAL' as ArticleCategory,
        status: 'DRAFT' as ArticleStatus,
        coverImage: '', authorName: 'Rédaction SimuLegal', authorRole: '',
        tags: '', metaTitle: '', metaDescription: '', featured: false,
    });
    const [isSaving, setIsSaving] = useState(false);
    const [previewMode, setPreviewMode] = useState(false);
    const [isUploading, setIsUploading] = useState(false);
    const textareaRef = useRef<HTMLTextAreaElement | null>(null);
    const fileInputRef = useRef<HTMLInputElement | null>(null);
    const coverInputRef = useRef<HTMLInputElement | null>(null);

    // Data loading
    const loadData = async () => {
        setIsLoading(true);
        const [arts, comms] = await Promise.all([
            BlogStore.getAll(filterStatus),
            BlogStore.getPendingComments(),
        ]);
        setArticles(arts);
        setPendingComments(comms);
        setIsLoading(false);
    };

    const loadComments = async () => {
        setIsLoadingComments(true);
        const comms = await BlogStore.getPendingComments();
        setPendingComments(comms);
        setIsLoadingComments(false);
    };

    const loadVeilleData = async () => {
        setIsLoadingVeille(true);
        const [topics, stats, config] = await Promise.all([
            BlogStore.getAutoTopics(),
            BlogStore.getAutoStats(),
            BlogStore.getAutoConfig(),
        ]);
        setAutoTopics(topics);
        setAutoStats(stats);
        setAutoConfig(config);
        setIsLoadingVeille(false);
    };

    useEffect(() => { loadData(); }, [filterStatus]);

    const filteredArticles = articles.filter(a =>
        a.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
        (a.tags || '').toLowerCase().includes(searchQuery.toLowerCase())
    );

    const generateSlug = (title: string) =>
        title.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '').substring(0, 80);

    const openEditor = (article?: Article) => {
        if (article) {
            setEditingArticle(article);
            setForm({
                title: article.title, slug: article.slug,
                excerpt: article.excerpt || '', content: article.content,
                category: article.category as ArticleCategory,
                status: article.status as ArticleStatus,
                coverImage: article.coverImage || '',
                authorName: article.authorName, authorRole: article.authorRole || '',
                tags: article.tags || '', metaTitle: article.metaTitle || '',
                metaDescription: article.metaDescription || '', featured: article.featured,
            });
        } else {
            setEditingArticle(null);
            setForm({
                title: '', slug: '', excerpt: '', content: '',
                category: 'GENERAL', status: 'DRAFT', coverImage: '',
                authorName: 'Rédaction SimuLegal', authorRole: '',
                tags: '', metaTitle: '', metaDescription: '', featured: false,
            });
        }
        setPreviewMode(false);
        setShowEditor(true);
    };

    const handleSave = async () => {
        if (!form.title.trim() || !form.content.trim()) return;
        setIsSaving(true);
        const data = { ...form, slug: form.slug || generateSlug(form.title) };
        const result = editingArticle
            ? await BlogStore.update(editingArticle.id, data)
            : await BlogStore.create(data);
        if (result) { setShowEditor(false); loadData(); }
        setIsSaving(false);
    };

    const handleDelete = async (id: string) => {
        if (!confirm('Supprimer cet article ?')) return;
        await BlogStore.delete(id);
        loadData();
    };

    const handleToggleStatus = async (article: Article) => {
        await BlogStore.update(article.id, { status: article.status === 'PUBLISHED' ? 'DRAFT' : 'PUBLISHED' });
        loadData();
    };

    const handleToggleFeatured = async (article: Article) => {
        await BlogStore.update(article.id, { featured: !article.featured });
        loadData();
    };

    const handleModerateComment = async (id: string, status: 'APPROVED' | 'REJECTED') => {
        await BlogStore.moderateComment(id, status);
        loadComments();
    };

    const handleDeleteComment = async (id: string) => {
        if (!confirm('Supprimer définitivement ce commentaire ?')) return;
        await BlogStore.deleteComment(id);
        loadComments();
    };

    // Image upload handlers
    const handleImageUpload = useCallback(async (file: File, target: 'content' | 'cover') => {
        setIsUploading(true);
        const result = await BlogStore.uploadImage(file);
        if (result) {
            if (target === 'content') {
                const mdImg = `![${file.name}](${result.url})`;
                const ta = textareaRef.current;
                if (ta) {
                    const pos = ta.selectionStart;
                    const newContent = form.content.substring(0, pos) + '\n' + mdImg + '\n' + form.content.substring(pos);
                    setForm(f => ({ ...f, content: newContent }));
                } else {
                    setForm(f => ({ ...f, content: f.content + '\n' + mdImg + '\n' }));
                }
            } else {
                setForm(f => ({ ...f, coverImage: result.url }));
            }
        }
        setIsUploading(false);
    }, [form.content]);

    const onContentFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (file) handleImageUpload(file, 'content');
        e.target.value = '';
    };

    const onCoverFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (file) handleImageUpload(file, 'cover');
        e.target.value = '';
    };

    // Keyboard shortcuts
    const handleKeyDown = (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
        if (e.ctrlKey || e.metaKey) {
            if (e.key === 'b') {
                e.preventDefault();
                const ta = textareaRef.current;
                if (!ta) return;
                const start = ta.selectionStart;
                const end = ta.selectionEnd;
                const sel = form.content.substring(start, end);
                const newContent = form.content.substring(0, start) + '**' + (sel || 'texte') + '**' + form.content.substring(end);
                setForm(f => ({ ...f, content: newContent }));
            }
            if (e.key === 's') {
                e.preventDefault();
                handleSave();
            }
        }
    };

    const totalArticles = articles.length;
    const publishedCount = articles.filter(a => a.status === 'PUBLISHED').length;
    const totalViews = articles.reduce((sum, a) => sum + a.viewCount, 0);
    const featuredCount = articles.filter(a => a.featured).length;

    // ════════════════════════════════════
    // EDITOR VIEW
    // ════════════════════════════════════
    if (showEditor) {
        return (
            <div className="min-h-screen bg-slate-50 p-6">
                <div className="max-w-5xl mx-auto">
                    {/* Hidden file inputs */}
                    <input ref={fileInputRef} type="file" accept="image/*" className="hidden" onChange={onContentFileSelect} />
                    <input ref={coverInputRef} type="file" accept="image/*" className="hidden" onChange={onCoverFileSelect} />

                    {/* Header */}
                    <div className="flex items-center justify-between mb-8">
                        <button onClick={() => setShowEditor(false)} className="flex items-center gap-2 text-slate-500 hover:text-slate-800 font-bold transition-colors">
                            <ArrowLeft size={18} /> Retour
                        </button>
                        <div className="flex items-center gap-3">
                            {isUploading && <span className="text-xs text-indigo-500 font-bold flex items-center gap-1"><Loader2 size={14} className="animate-spin" /> Upload...</span>}
                            <select value={form.status} onChange={(e) => setForm(f => ({ ...f, status: e.target.value as ArticleStatus }))} className="px-4 py-2 rounded-xl border border-slate-200 text-sm font-bold bg-white outline-none">
                                <option value="DRAFT">🟡 Brouillon</option>
                                <option value="PUBLISHED">🟢 Publier</option>
                                <option value="ARCHIVED">⬜ Archiver</option>
                            </select>
                            <button onClick={handleSave} disabled={isSaving || !form.title.trim() || !form.content.trim()} className="px-6 py-2.5 bg-indigo-600 hover:bg-indigo-700 disabled:bg-slate-300 text-white font-bold rounded-xl flex items-center gap-2 transition-all shadow-lg shadow-indigo-200">
                                {isSaving ? <Loader2 size={16} className="animate-spin" /> : <Save size={16} />}
                                {editingArticle ? 'Mettre à jour' : 'Créer'}
                            </button>
                        </div>
                    </div>

                    <div className="space-y-6">
                        {/* Title */}
                        <div>
                            <input type="text" value={form.title} onChange={(e) => {
                                const title = e.target.value;
                                setForm(f => ({ ...f, title, slug: f.slug === generateSlug(f.title) || !f.slug ? generateSlug(title) : f.slug }));
                            }} placeholder="Titre de l'article..." className="w-full text-3xl font-black text-slate-900 bg-transparent border-none outline-none placeholder:text-slate-300" autoFocus />
                            <div className="flex items-center gap-2 mt-2">
                                <Globe size={14} className="text-slate-400" />
                                <input type="text" value={form.slug} onChange={(e) => setForm(f => ({ ...f, slug: e.target.value }))} placeholder="slug-url" className="text-xs font-mono text-slate-400 bg-transparent border-none outline-none flex-1" />
                            </div>
                        </div>

                        {/* Category + Author */}
                        <div className="grid grid-cols-2 gap-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">Catégorie</label>
                                <select value={form.category} onChange={(e) => setForm(f => ({ ...f, category: e.target.value as ArticleCategory }))} className="w-full px-4 py-3 rounded-xl border border-slate-200 bg-white font-medium text-sm outline-none">
                                    {CATEGORIES.filter(c => c.id !== 'ALL').map(c => <option key={c.id} value={c.id}>{c.icon} {c.label}</option>)}
                                </select>
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">Auteur</label>
                                <input type="text" value={form.authorName} onChange={(e) => setForm(f => ({ ...f, authorName: e.target.value }))} className="w-full px-4 py-3 rounded-xl border border-slate-200 bg-white font-medium text-sm outline-none" />
                            </div>
                        </div>

                        {/* Excerpt */}
                        <div>
                            <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">Résumé (extrait)</label>
                            <textarea value={form.excerpt} onChange={(e) => setForm(f => ({ ...f, excerpt: e.target.value }))} placeholder="Un court résumé pour les cartes et le SEO..." rows={2} className="w-full px-4 py-3 rounded-xl border border-slate-200 bg-white font-medium text-sm resize-none outline-none" />
                        </div>

                        {/* Content Editor */}
                        <div>
                            <div className="flex items-center justify-between mb-2">
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider">Contenu (Markdown)</label>
                                <div className="flex bg-slate-200 p-0.5 rounded-lg">
                                    <button type="button" onClick={() => setPreviewMode(false)} className={`px-3 py-1 text-xs font-bold rounded-md transition-all ${!previewMode ? 'bg-white text-slate-800 shadow-sm' : 'text-slate-500 hover:text-slate-700'}`}>
                                        ✏️ Éditeur
                                    </button>
                                    <button type="button" onClick={() => setPreviewMode(true)} className={`px-3 py-1 text-xs font-bold rounded-md transition-all ${previewMode ? 'bg-white text-slate-800 shadow-sm' : 'text-slate-500 hover:text-slate-700'}`}>
                                        👁️ Aperçu
                                    </button>
                                </div>
                            </div>

                            {!previewMode ? (
                                <div className="space-y-2">
                                    {/* Toolbar */}
                                    <MarkdownToolbar
                                        textareaRef={textareaRef}
                                        content={form.content}
                                        onChange={(val) => setForm(f => ({ ...f, content: val }))}
                                        onUploadImage={() => fileInputRef.current?.click()}
                                    />
                                    {/* Textarea */}
                                    <textarea
                                        ref={textareaRef}
                                        value={form.content}
                                        onChange={(e) => setForm(f => ({ ...f, content: e.target.value }))}
                                        onKeyDown={handleKeyDown}
                                        placeholder="Rédigez votre article en Markdown (titres avec ##, gras avec **, listes avec -)..."
                                        rows={22}
                                        className="w-full px-4 py-4 rounded-xl border border-slate-200 bg-white font-mono text-sm resize-y leading-relaxed outline-none focus:ring-2 focus:ring-indigo-100 focus:border-indigo-400 transition-all"
                                    />
                                </div>
                            ) : (
                                <div
                                    className="w-full px-6 py-6 rounded-xl border border-slate-200 bg-white text-slate-800 text-sm leading-relaxed overflow-y-auto min-h-[500px] prose-preview"
                                    dangerouslySetInnerHTML={{ __html: `<p style="line-height:1.7; color:#475569;">${renderMarkdownPreview(form.content)}</p>` }}
                                />
                            )}
                        </div>

                        {/* Tags + Cover Image */}
                        <div className="grid grid-cols-2 gap-4">
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                                    <Tag size={12} className="inline mr-1" /> Tags (séparés par des virgules)
                                </label>
                                <input type="text" value={form.tags} onChange={(e) => setForm(f => ({ ...f, tags: e.target.value }))} placeholder="immigration, titre de séjour, réforme 2026" className="w-full px-4 py-3 rounded-xl border border-slate-200 bg-white font-medium text-sm outline-none" />
                            </div>
                            <div>
                                <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">Image de couverture</label>
                                <div className="flex items-center gap-3">
                                    <input type="text" value={form.coverImage} onChange={(e) => setForm(f => ({ ...f, coverImage: e.target.value }))} placeholder="https://..." className="flex-1 px-4 py-3 rounded-xl border border-slate-200 bg-white font-medium text-sm outline-none" />
                                    <button type="button" onClick={() => coverInputRef.current?.click()} className="px-4 py-3 rounded-xl border border-indigo-200 bg-indigo-50 text-indigo-600 hover:bg-indigo-100 font-bold text-sm transition-colors flex items-center gap-1.5">
                                        <Upload size={14} /> Upload
                                    </button>
                                    {form.coverImage && (
                                        <div className="w-12 h-12 flex-shrink-0 border border-slate-200 rounded-lg overflow-hidden">
                                            <img src={form.coverImage} alt="Cover" className="w-full h-full object-cover" />
                                        </div>
                                    )}
                                </div>
                            </div>
                        </div>

                        {/* SEO Section */}
                        <details className="bg-white rounded-2xl border border-slate-200 p-5 mt-4">
                            <summary className="text-sm font-bold text-slate-600 cursor-pointer flex items-center gap-2 outline-none">
                                <ChevronDown size={16} className="text-slate-400" />
                                Paramètres SEO & Affichage
                            </summary>
                            <div className="mt-4 space-y-4">
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 mb-1">Meta Title</label>
                                    <input type="text" value={form.metaTitle} onChange={(e) => setForm(f => ({ ...f, metaTitle: e.target.value }))} placeholder={form.title || 'Titre SEO...'} className="w-full px-4 py-2 rounded-lg border border-slate-200 text-sm outline-none" />
                                </div>
                                <div>
                                    <label className="block text-xs font-bold text-slate-500 mb-1">Meta Description</label>
                                    <textarea value={form.metaDescription} onChange={(e) => setForm(f => ({ ...f, metaDescription: e.target.value }))} placeholder={form.excerpt || 'Description SEO...'} rows={2} className="w-full px-4 py-2 rounded-lg border border-slate-200 text-sm resize-none outline-none" />
                                </div>
                                <div className="flex items-center gap-3 w-full p-4 bg-amber-50 rounded-xl border border-amber-100">
                                    <input type="checkbox" id="featured" checked={form.featured} onChange={(e) => setForm(f => ({ ...f, featured: e.target.checked }))} className="w-5 h-5 text-amber-500 rounded focus:ring-0 cursor-pointer" />
                                    <label htmlFor="featured" className="text-sm font-bold text-amber-900 cursor-pointer">
                                        ⭐ Définir comme article mis en vedette (affiché en haut de la page blog)
                                    </label>
                                </div>
                            </div>
                        </details>
                    </div>
                </div>
            </div>
        );
    }

    // ════════════════════════════════════
    // LIST VIEW
    // ════════════════════════════════════
    return (
        <div className="min-h-screen bg-slate-50 p-6">
            <div className="max-w-6xl mx-auto">
                {/* Header */}
                <div className="flex items-center justify-between mb-8">
                    <div>
                        <h1 className="text-2xl font-black text-slate-900">Blog & Insights</h1>
                        <p className="text-sm text-slate-400 font-medium">Gérez vos articles, publications, et commentaires</p>
                    </div>
                    <button onClick={() => openEditor()} className="px-5 py-2.5 bg-indigo-600 hover:bg-indigo-700 text-white font-bold rounded-xl flex items-center gap-2 transition-all shadow-lg shadow-indigo-200">
                        <Plus size={18} /> Nouvel article
                    </button>
                </div>

                {/* Tabs */}
                <div className="flex gap-6 border-b border-slate-200 mb-8">
                    <button onClick={() => setActiveTab('ARTICLES')} className={`pb-3 font-bold text-sm transition-colors relative ${activeTab === 'ARTICLES' ? 'text-indigo-600' : 'text-slate-500 hover:text-slate-800'}`}>
                        📝 Articles
                        {activeTab === 'ARTICLES' && <span className="absolute bottom-0 left-0 w-full h-0.5 bg-indigo-600 rounded-t-xl" />}
                    </button>
                    <button onClick={() => setActiveTab('COMMENTS')} className={`pb-3 font-bold text-sm transition-colors relative flex items-center gap-2 ${activeTab === 'COMMENTS' ? 'text-indigo-600' : 'text-slate-500 hover:text-slate-800'}`}>
                        💬 Modération
                        {pendingComments.length > 0 && (
                            <span className="bg-rose-500 text-white text-[10px] px-1.5 py-0.5 rounded-full font-black">{pendingComments.length}</span>
                        )}
                        {activeTab === 'COMMENTS' && <span className="absolute bottom-0 left-0 w-full h-0.5 bg-indigo-600 rounded-t-xl" />}
                    </button>
                    <button onClick={() => { setActiveTab('VEILLE'); loadVeilleData(); }} className={`pb-3 font-bold text-sm transition-colors relative flex items-center gap-2 ${activeTab === 'VEILLE' ? 'text-indigo-600' : 'text-slate-500 hover:text-slate-800'}`}>
                        🤖 Veille Auto
                        {autoStats && autoStats.discovered > 0 && (
                            <span className="bg-purple-500 text-white text-[10px] px-1.5 py-0.5 rounded-full font-black">{autoStats.discovered}</span>
                        )}
                        {activeTab === 'VEILLE' && <span className="absolute bottom-0 left-0 w-full h-0.5 bg-indigo-600 rounded-t-xl" />}
                    </button>
                </div>

                {activeTab === 'ARTICLES' && (
                    <>
                        {/* Stats */}
                        <div className="grid grid-cols-4 gap-4 mb-8">
                            {[
                                { label: 'Total articles', value: totalArticles, icon: FileText, color: 'indigo' },
                                { label: 'Publiés', value: publishedCount, icon: Eye, color: 'emerald' },
                                { label: 'Vues totales', value: totalViews, icon: BarChart3, color: 'blue' },
                                { label: 'En vedette', value: featuredCount, icon: Star, color: 'amber' },
                            ].map(stat => {
                                const colors = STAT_COLORS[stat.color] || STAT_COLORS.indigo;
                                return (
                                    <div key={stat.label} className="bg-white rounded-2xl p-5 border border-slate-100 shadow-sm hover:shadow-md transition-all">
                                        <div className="flex items-center gap-3 mb-2">
                                            <div className={`w-10 h-10 rounded-xl ${colors.bg} flex items-center justify-center`}>
                                                <stat.icon size={20} className={colors.text} />
                                            </div>
                                        </div>
                                        <p className="text-3xl font-black text-slate-800">{stat.value}</p>
                                        <p className="text-xs font-bold text-slate-400">{stat.label}</p>
                                    </div>
                                );
                            })}
                        </div>

                        {/* Filters */}
                        <div className="flex items-center gap-4 mb-6">
                            <div className="relative flex-1 max-w-sm">
                                <Search className="absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" size={18} />
                                <input type="text" placeholder="Rechercher un article..." value={searchQuery} onChange={(e) => setSearchQuery(e.target.value)} className="w-full pl-10 pr-4 py-2.5 rounded-xl border border-slate-200 bg-white text-sm font-bold focus:ring-2 focus:ring-indigo-200 outline-none text-slate-700" />
                            </div>
                            <div className="flex gap-1 bg-white border border-slate-200 rounded-xl p-1">
                                {['ALL', 'DRAFT', 'PUBLISHED', 'ARCHIVED'].map(s => (
                                    <button key={s} onClick={() => setFilterStatus(s)} className={`px-4 py-1.5 rounded-lg text-xs font-black tracking-wider uppercase transition-all ${filterStatus === s ? 'bg-indigo-600 text-white shadow-sm' : 'text-slate-400 hover:bg-slate-50 hover:text-slate-600'}`}>
                                        {s === 'ALL' ? 'Tous' : STATUS_BADGES[s as ArticleStatus]?.label || s}
                                    </button>
                                ))}
                            </div>
                        </div>

                        {/* Articles List */}
                        {isLoading ? (
                            <div className="flex items-center justify-center py-20"><Loader2 size={32} className="animate-spin text-slate-300" /></div>
                        ) : filteredArticles.length === 0 ? (
                            <div className="text-center py-20 bg-white rounded-2xl border border-slate-100 border-dashed">
                                <FileText size={48} className="mx-auto text-slate-200 mb-4" />
                                <p className="text-slate-400 font-bold">Aucun article trouvé</p>
                                <button onClick={() => openEditor()} className="mt-4 text-indigo-600 font-bold text-sm hover:underline">Créer votre premier article →</button>
                            </div>
                        ) : (
                            <div className="space-y-3">
                                {filteredArticles.map(article => {
                                    const badge = STATUS_BADGES[article.status as ArticleStatus] || STATUS_BADGES.DRAFT;
                                    const cat = CATEGORIES.find(c => c.id === article.category);
                                    return (
                                        <div key={article.id} className="bg-white rounded-2xl border border-slate-100 p-5 flex items-center gap-5 hover:shadow-md transition-all group">
                                            <div className="w-16 h-16 rounded-xl bg-gradient-to-br from-indigo-50 to-purple-50 flex items-center justify-center shrink-0 overflow-hidden border border-slate-100">
                                                {article.coverImage ? <img src={article.coverImage} alt="" className="w-full h-full object-cover" /> : <span className="text-2xl">{cat?.icon || '📰'}</span>}
                                            </div>
                                            <div className="flex-1 min-w-0">
                                                <div className="flex items-center gap-3 mb-1">
                                                    <h3 className="font-bold text-slate-900 truncate">{article.title}</h3>
                                                    {article.featured && <Star size={14} className="text-amber-500 fill-amber-500 shrink-0" />}
                                                </div>
                                                <div className="flex flex-wrap items-center gap-3 text-xs text-slate-400 font-medium mt-2">
                                                    <span className={`px-2 py-0.5 rounded-md font-bold text-[10px] uppercase tracking-wider ${badge.cls}`}>{badge.label}</span>
                                                    <span className="flex items-center gap-1 font-bold"><Tag size={12} className="text-slate-300" /> {cat?.label}</span>
                                                    <span className="flex items-center gap-1 font-bold"><Clock size={12} className="text-slate-300" /> {article.readTimeMin} min</span>
                                                    <span className="flex items-center gap-1 font-bold"><Eye size={12} className="text-slate-300" /> {article.viewCount} vues</span>
                                                    {article._count && article._count.comments > 0 && (
                                                        <span className="flex items-center gap-1 font-bold text-indigo-500"><MessageCircle size={12} /> {article._count.comments}</span>
                                                    )}
                                                    {article.publishedAt && (
                                                        <span className="flex items-center gap-1 font-bold ml-auto"><Calendar size={12} className="text-slate-300" />{new Date(article.publishedAt).toLocaleDateString('fr-FR')}</span>
                                                    )}
                                                </div>
                                            </div>
                                            <div className="flex items-center gap-1.5 opacity-0 group-hover:opacity-100 transition-opacity shrink-0">
                                                <button onClick={() => handleToggleFeatured(article)} title={article.featured ? 'Retirer de la vedette' : 'Mettre en vedette'} className="p-2 rounded-xl bg-slate-50 hover:bg-amber-50 text-slate-400 hover:text-amber-500 transition-colors">
                                                    {article.featured ? <StarOff size={16} /> : <Star size={16} />}
                                                </button>
                                                <button onClick={() => handleToggleStatus(article)} title={article.status === 'PUBLISHED' ? 'Dépublier' : 'Publier'} className="p-2 rounded-xl bg-slate-50 hover:bg-emerald-50 text-slate-400 hover:text-emerald-600 transition-colors">
                                                    {article.status === 'PUBLISHED' ? <EyeOff size={16} /> : <Eye size={16} />}
                                                </button>
                                                <button onClick={() => openEditor(article)} className="p-2 rounded-xl bg-slate-50 hover:bg-indigo-50 text-slate-400 hover:text-indigo-600 transition-colors border border-slate-100">
                                                    <Edit3 size={16} />
                                                </button>
                                                <button onClick={() => handleDelete(article.id)} className="p-2 rounded-xl bg-slate-50 hover:bg-rose-50 text-slate-400 hover:text-rose-600 transition-colors border border-slate-100">
                                                    <Trash2 size={16} />
                                                </button>
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>
                        )}
                    </>
                )}

                {activeTab === 'COMMENTS' && (
                    <div className="space-y-4">
                        <div className="bg-indigo-50 text-indigo-700 p-4 rounded-2xl text-sm font-bold flex flex-col sm:flex-row shadow-sm border border-indigo-100 items-center justify-between">
                            <span>Tous les commentaires soumis par les visiteurs doivent être approuvés avant d&apos;être visibles publiquement.</span>
                            <span className="bg-indigo-600 text-white px-3 py-1 rounded-full text-xs shrink-0 mt-3 sm:mt-0">{pendingComments.length} en attente</span>
                        </div>

                        {isLoadingComments ? (
                            <div className="flex items-center justify-center py-20"><Loader2 size={32} className="animate-spin text-slate-300" /></div>
                        ) : pendingComments.length === 0 ? (
                            <div className="text-center py-20 bg-white rounded-2xl border border-slate-100 border-dashed">
                                <CheckCircle size={48} className="mx-auto text-emerald-300 mb-4" />
                                <p className="text-slate-500 font-bold">Aucun commentaire en attente</p>
                                <p className="text-slate-400 text-sm mt-1">Vous êtes à jour !</p>
                            </div>
                        ) : (
                            pendingComments.map(comment => (
                                <div key={comment.id} className="bg-white rounded-2xl border border-rose-100 p-5 shadow-sm relative">
                                    <div className="absolute top-0 right-0 px-3 py-1 bg-amber-100 text-amber-700 text-[10px] font-black uppercase rounded-bl-xl rounded-tr-2xl">En attente</div>
                                    <div className="flex gap-4 mb-4">
                                        <div className="w-10 h-10 rounded-full bg-slate-100 flex items-center justify-center text-slate-400 font-black shrink-0">
                                            {comment.authorName.charAt(0).toUpperCase()}
                                        </div>
                                        <div>
                                            <h4 className="font-bold text-slate-800">{comment.authorName}</h4>
                                            {comment.authorEmail && <p className="text-xs text-slate-400">{comment.authorEmail}</p>}
                                            <p className="text-[10px] text-slate-300 mt-1 font-bold uppercase">Le {new Date(comment.createdAt).toLocaleDateString('fr-FR')}</p>
                                        </div>
                                    </div>
                                    <div className="pl-14 pr-4">
                                        <p className="text-slate-700 font-medium mb-4 italic text-sm">&ldquo;{comment.content}&rdquo;</p>
                                        <div className="bg-slate-50 rounded-xl p-3 border border-slate-100 text-xs font-bold text-slate-500 flex items-center gap-2 mb-6">
                                            <FileText size={14} className="text-indigo-400" />
                                            Article : <span className="text-indigo-600">{comment.article?.title}</span>
                                        </div>
                                        <div className="flex items-center gap-3 border-t border-slate-50 pt-4">
                                            <button onClick={() => handleModerateComment(comment.id, 'APPROVED')} className="px-4 py-2 bg-emerald-50 text-emerald-600 hover:bg-emerald-600 hover:text-white rounded-xl font-bold flex items-center gap-2 transition-colors text-sm">
                                                <CheckCircle size={16} /> Approuver
                                            </button>
                                            <button onClick={() => handleModerateComment(comment.id, 'REJECTED')} className="px-4 py-2 bg-rose-50 text-rose-600 hover:bg-rose-600 hover:text-white rounded-xl font-bold flex items-center gap-2 transition-colors text-sm">
                                                <XCircle size={16} /> Refuser
                                            </button>
                                            <button onClick={() => handleDeleteComment(comment.id)} className="px-4 py-2 text-slate-400 hover:text-rose-600 hover:bg-rose-50 rounded-xl font-bold flex items-center gap-2 transition-colors text-sm ml-auto">
                                                <Trash2 size={16} />
                                            </button>
                                        </div>
                                    </div>
                                </div>
                            ))
                        )}
                    </div>
                )}

                {/* ═══════════════════════════════════════════ */}
                {/* TAB: VEILLE AUTO                          */}
                {/* ═══════════════════════════════════════════ */}
                {activeTab === 'VEILLE' && (
                    <div className="space-y-6">
                        {/* ── Pipeline Stats ── */}
                        {autoStats && (
                            <div className="grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-3">
                                {[
                                    { label: 'Total sujets', value: autoStats.total, color: 'bg-slate-50 text-slate-600', icon: '📊' },
                                    { label: 'Découverts', value: autoStats.discovered, color: 'bg-blue-50 text-blue-600', icon: '🔍' },
                                    { label: 'En cours', value: autoStats.generating, color: 'bg-amber-50 text-amber-600', icon: '⏳' },
                                    { label: 'Générés', value: autoStats.generated, color: 'bg-emerald-50 text-emerald-600', icon: '✅' },
                                    { label: 'Rejetés', value: autoStats.rejected, color: 'bg-rose-50 text-rose-600', icon: '❌' },
                                    { label: 'Publiés', value: autoStats.published, color: 'bg-purple-50 text-purple-600', icon: '🚀' },
                                ].map(s => (
                                    <div key={s.label} className={`${s.color} rounded-2xl p-4 text-center`}>
                                        <div className="text-xl mb-1">{s.icon}</div>
                                        <div className="text-2xl font-black">{s.value}</div>
                                        <div className="text-xs font-bold opacity-70">{s.label}</div>
                                    </div>
                                ))}
                            </div>
                        )}

                        {/* ── Action Bar ── */}
                        <div className="flex flex-wrap items-center gap-3 bg-gradient-to-r from-purple-50 to-indigo-50 p-4 rounded-2xl border border-purple-100">
                            <div className="flex-1 min-w-0">
                                <h3 className="font-bold text-slate-800 text-sm">Pipeline de veille légale</h3>
                                <p className="text-xs text-slate-500 mt-0.5">
                                    {autoConfig?.enabled ? '✅ Actif' : '⏸ Désactivé'}
                                    {autoConfig?.lastRunAt && ` — Dernière exécution : ${new Date(autoConfig.lastRunAt).toLocaleDateString('fr-FR', { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })}`}
                                    {autoConfig && ` — ${autoConfig.totalGenerated} articles générés au total`}
                                </p>
                            </div>
                            <button
                                onClick={async () => { setIsPipelineRunning(true); setPipelineResult(null); const r = await BlogStore.triggerPipeline(); setIsPipelineRunning(false); if (r) setPipelineResult(`✅ ${r.discovered} sujets découverts, ${r.generated} articles générés${r.errors > 0 ? `, ${r.errors} erreurs` : ''}`); await loadVeilleData(); }}
                                disabled={isPipelineRunning}
                                className="px-5 py-2.5 bg-purple-600 hover:bg-purple-700 text-white font-bold rounded-xl flex items-center gap-2 transition-all shadow-lg shadow-purple-200 disabled:opacity-50 text-sm"
                            >
                                {isPipelineRunning ? <Loader2 size={16} className="animate-spin" /> : <Zap size={16} />}
                                {isPipelineRunning ? 'Pipeline en cours...' : 'Lancer le pipeline'}
                            </button>
                            <button
                                onClick={async () => { setIsPipelineRunning(true); await BlogStore.discoverTopics(); setIsPipelineRunning(false); await loadVeilleData(); setPipelineResult('🔍 Découverte terminée'); }}
                                disabled={isPipelineRunning}
                                className="px-4 py-2.5 bg-white text-blue-600 border border-blue-200 font-bold rounded-xl flex items-center gap-2 hover:bg-blue-50 transition-all text-sm disabled:opacity-50"
                            >
                                <Search size={14} /> Découvrir
                            </button>
                            <button
                                onClick={async () => { setIsPipelineRunning(true); const r = await BlogStore.generateDrafts(); setIsPipelineRunning(false); if (r) setPipelineResult(`🤖 ${r.generated} articles générés`); await loadVeilleData(); }}
                                disabled={isPipelineRunning}
                                className="px-4 py-2.5 bg-white text-emerald-600 border border-emerald-200 font-bold rounded-xl flex items-center gap-2 hover:bg-emerald-50 transition-all text-sm disabled:opacity-50"
                            >
                                <Sparkles size={14} /> Générer
                            </button>
                            <button
                                onClick={() => setShowConfigPanel(!showConfigPanel)}
                                className="p-2.5 bg-white text-slate-600 border border-slate-200 rounded-xl hover:bg-slate-50 transition-all"
                                title="Configuration"
                            >
                                <Settings size={16} />
                            </button>
                        </div>

                        {/* Toast */}
                        {pipelineResult && (
                            <div className="bg-emerald-50 text-emerald-700 px-4 py-3 rounded-xl text-sm font-bold flex items-center justify-between border border-emerald-100">
                                <span>{pipelineResult}</span>
                                <button onClick={() => setPipelineResult(null)} className="text-emerald-400 hover:text-emerald-600"><X size={14} /></button>
                            </div>
                        )}

                        {/* ── Config Panel ── */}
                        {showConfigPanel && autoConfig && (
                            <div className="bg-white rounded-2xl border border-slate-200 p-6 shadow-sm">
                                <h3 className="font-black text-slate-800 mb-4 flex items-center gap-2"><Settings size={18} className="text-indigo-500" /> Configuration du pipeline</h3>
                                <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-4">
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Statut</label>
                                        <button
                                            onClick={async () => { await BlogStore.updateAutoConfig({ enabled: !autoConfig.enabled }); const c = await BlogStore.getAutoConfig(); if (c) setAutoConfig(c); }}
                                            className={`w-full px-4 py-2.5 rounded-xl font-bold text-sm transition-all ${autoConfig.enabled ? 'bg-emerald-100 text-emerald-700 hover:bg-emerald-200' : 'bg-slate-100 text-slate-500 hover:bg-slate-200'}`}
                                        >
                                            {autoConfig.enabled ? '✅ Pipeline actif' : '⏸ Pipeline désactivé'}
                                        </button>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Fréquence</label>
                                        <select
                                            value={autoConfig.frequency}
                                            onChange={async (e) => { await BlogStore.updateAutoConfig({ frequency: e.target.value }); const c = await BlogStore.getAutoConfig(); if (c) setAutoConfig(c); }}
                                            className="w-full px-4 py-2.5 rounded-xl border border-slate-200 font-bold text-sm focus:ring-2 focus:ring-indigo-300 outline-none"
                                        >
                                            <option value="DAILY">Quotidien</option>
                                            <option value="WEEKLY">Hebdomadaire</option>
                                            <option value="BIWEEKLY">Bimensuel</option>
                                            <option value="MONTHLY">Mensuel</option>
                                        </select>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Articles max par exécution</label>
                                        <input
                                            type="number" min={1} max={10} value={autoConfig.maxArticlesPerRun}
                                            onChange={async (e) => { await BlogStore.updateAutoConfig({ maxArticlesPerRun: Number(e.target.value) }); const c = await BlogStore.getAutoConfig(); if (c) setAutoConfig(c); }}
                                            className="w-full px-4 py-2.5 rounded-xl border border-slate-200 font-bold text-sm focus:ring-2 focus:ring-indigo-300 outline-none"
                                        />
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Score min de pertinence (0-100)</label>
                                        <input
                                            type="number" min={0} max={100} value={autoConfig.minRelevanceScore}
                                            onChange={async (e) => { await BlogStore.updateAutoConfig({ minRelevanceScore: Number(e.target.value) }); const c = await BlogStore.getAutoConfig(); if (c) setAutoConfig(c); }}
                                            className="w-full px-4 py-2.5 rounded-xl border border-slate-200 font-bold text-sm focus:ring-2 focus:ring-indigo-300 outline-none"
                                        />
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Modèle IA</label>
                                        <select
                                            value={autoConfig.aiModel}
                                            onChange={async (e) => { await BlogStore.updateAutoConfig({ aiModel: e.target.value }); const c = await BlogStore.getAutoConfig(); if (c) setAutoConfig(c); }}
                                            className="w-full px-4 py-2.5 rounded-xl border border-slate-200 font-bold text-sm focus:ring-2 focus:ring-indigo-300 outline-none"
                                        >
                                            <option value="gpt-4o-mini">GPT-4o Mini (rapide)</option>
                                            <option value="gpt-4o">GPT-4o (qualité)</option>
                                            <option value="gpt-4-turbo">GPT-4 Turbo</option>
                                        </select>
                                    </div>
                                    <div>
                                        <label className="block text-xs font-bold text-slate-500 mb-1">Catégories ciblées</label>
                                        <input
                                            type="text" value={autoConfig.targetCategories}
                                            onChange={async (e) => { await BlogStore.updateAutoConfig({ targetCategories: e.target.value }); const c = await BlogStore.getAutoConfig(); if (c) setAutoConfig(c); }}
                                            className="w-full px-4 py-2.5 rounded-xl border border-slate-200 font-bold text-sm focus:ring-2 focus:ring-indigo-300 outline-none"
                                            placeholder="IMMIGRATION,NATURALISATION,..."
                                        />
                                    </div>
                                </div>
                            </div>
                        )}

                        {/* ── Topic Filter ── */}
                        <div className="flex gap-2 flex-wrap">
                            {[
                                { id: 'ALL', label: 'Tous', count: autoTopics.length },
                                { id: 'DISCOVERED', label: '🔍 Découverts', count: autoTopics.filter(t => t.status === 'DISCOVERED').length },
                                { id: 'GENERATED', label: '✅ Générés', count: autoTopics.filter(t => t.status === 'GENERATED').length },
                                { id: 'REJECTED', label: '❌ Rejetés', count: autoTopics.filter(t => t.status === 'REJECTED').length },
                            ].map(f => (
                                <button key={f.id} onClick={() => setTopicFilter(f.id)}
                                    className={`px-3 py-1.5 rounded-lg text-xs font-bold transition-all ${topicFilter === f.id ? 'bg-indigo-600 text-white' : 'bg-white text-slate-500 border border-slate-200 hover:border-indigo-300'}`}>
                                    {f.label} ({f.count})
                                </button>
                            ))}
                        </div>

                        {/* ── Topics List ── */}
                        {isLoadingVeille ? (
                            <div className="flex items-center justify-center py-20"><Loader2 size={32} className="animate-spin text-slate-300" /></div>
                        ) : autoTopics.filter(t => topicFilter === 'ALL' || t.status === topicFilter).length === 0 ? (
                            <div className="text-center py-20 bg-white rounded-2xl border border-dashed border-slate-200">
                                <Bot size={48} className="mx-auto text-purple-200 mb-4" />
                                <p className="text-slate-500 font-bold">Aucun sujet découvert</p>
                                <p className="text-slate-400 text-sm mt-1">Lancez le pipeline pour découvrir des sujets juridiques.</p>
                            </div>
                        ) : (
                            <div className="space-y-3">
                                {autoTopics.filter(t => topicFilter === 'ALL' || t.status === topicFilter).map(topic => {
                                    const statusMap: Record<string, { bg: string; text: string; label: string }> = {
                                        DISCOVERED: { bg: 'bg-blue-100', text: 'text-blue-700', label: 'Découvert' },
                                        GENERATING: { bg: 'bg-amber-100', text: 'text-amber-700', label: 'Génération...' },
                                        GENERATED: { bg: 'bg-emerald-100', text: 'text-emerald-700', label: 'Article créé' },
                                        REJECTED: { bg: 'bg-rose-100', text: 'text-rose-600', label: 'Rejeté' },
                                        PUBLISHED: { bg: 'bg-purple-100', text: 'text-purple-700', label: 'Publié' },
                                    };
                                    const st = statusMap[topic.status] || statusMap.DISCOVERED;
                                    const catInfo = CATEGORIES.find(c => c.id === topic.category);

                                    return (
                                        <div key={topic.id} className="bg-white rounded-2xl border border-slate-200 p-5 hover:shadow-md transition-all">
                                            <div className="flex items-start gap-4">
                                                {/* Relevance Score */}
                                                <div className="shrink-0 w-14 text-center">
                                                    <div className={`text-lg font-black ${topic.relevanceScore >= 70 ? 'text-emerald-600' : topic.relevanceScore >= 50 ? 'text-amber-600' : 'text-slate-400'}`}>
                                                        {topic.relevanceScore}
                                                    </div>
                                                    <div className="w-full bg-slate-100 rounded-full h-1.5 mt-1">
                                                        <div className={`h-1.5 rounded-full transition-all ${topic.relevanceScore >= 70 ? 'bg-emerald-500' : topic.relevanceScore >= 50 ? 'bg-amber-500' : 'bg-slate-300'}`}
                                                            style={{ width: `${topic.relevanceScore}%` }} />
                                                    </div>
                                                    <div className="text-[9px] font-bold text-slate-400 mt-0.5">pertinence</div>
                                                </div>

                                                {/* Content */}
                                                <div className="flex-1 min-w-0">
                                                    <div className="flex items-center gap-2 mb-1 flex-wrap">
                                                        <span className={`px-2 py-0.5 rounded-full text-[10px] font-black ${st.bg} ${st.text}`}>{st.label}</span>
                                                        {catInfo && <span className="text-xs text-slate-400">{catInfo.icon} {catInfo.label}</span>}
                                                        {topic.sourceName && <span className="text-[10px] text-slate-300 font-bold">via {topic.sourceName}</span>}
                                                    </div>
                                                    <h4 className="font-bold text-slate-800 text-sm leading-tight mb-1">{topic.title}</h4>
                                                    <p className="text-xs text-slate-500 line-clamp-2">{topic.summary}</p>
                                                    {topic.keywords && (
                                                        <div className="flex gap-1 mt-2 flex-wrap">
                                                            {topic.keywords.split(',').filter(Boolean).map(kw => (
                                                                <span key={kw} className="text-[9px] bg-indigo-50 text-indigo-600 px-1.5 py-0.5 rounded font-bold">{kw}</span>
                                                            ))}
                                                        </div>
                                                    )}
                                                    {topic.error && (
                                                        <div className="mt-2 flex items-center gap-1 text-[10px] text-rose-500 font-bold">
                                                            <AlertTriangle size={10} /> {topic.error}
                                                        </div>
                                                    )}
                                                </div>

                                                {/* Actions */}
                                                <div className="flex items-center gap-2 shrink-0">
                                                    {topic.sourceUrl && (
                                                        <a href={topic.sourceUrl} target="_blank" rel="noopener noreferrer" title="Voir la source"
                                                            className="p-2 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded-xl transition-colors">
                                                            <ExternalLink size={14} />
                                                        </a>
                                                    )}
                                                    {topic.status === 'DISCOVERED' && (
                                                        <button
                                                            onClick={async () => { await BlogStore.rejectTopic(topic.id); await loadVeilleData(); }}
                                                            title="Rejeter"
                                                            className="p-2 text-slate-400 hover:text-rose-600 hover:bg-rose-50 rounded-xl transition-colors"
                                                        >
                                                            <XCircle size={14} />
                                                        </button>
                                                    )}
                                                    {topic.status === 'REJECTED' && (
                                                        <button
                                                            onClick={async () => { await BlogStore.retryTopic(topic.id); await loadVeilleData(); }}
                                                            title="Réessayer"
                                                            className="p-2 text-slate-400 hover:text-blue-600 hover:bg-blue-50 rounded-xl transition-colors"
                                                        >
                                                            <RotateCcw size={14} />
                                                        </button>
                                                    )}
                                                    {topic.generatedArticleId && (
                                                        <button
                                                            onClick={() => { const art = articles.find(a => a.id === topic.generatedArticleId); if (art) { setActiveTab('ARTICLES'); setSearchQuery(art.title.substring(0, 20)); } }}
                                                            title="Voir l'article généré"
                                                            className="p-2 text-emerald-400 hover:text-emerald-600 hover:bg-emerald-50 rounded-xl transition-colors"
                                                        >
                                                            <Eye size={14} />
                                                        </button>
                                                    )}
                                                </div>
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>
                        )}
                    </div>
                )}
            </div>
        </div>
    );
}
