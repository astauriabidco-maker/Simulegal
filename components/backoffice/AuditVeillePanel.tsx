'use client';

import React, { useState } from 'react';
import {
    Eye,
    Settings,
    Brain,
    Scale,
    AlertTriangle,
    Clock,
    ChevronRight,
    Bookmark,
    FileText,
    History,
    Zap
} from 'lucide-react';
import ServiceConfigPanel from './ServiceConfigPanel';
import EligibilityConfigPanel from './EligibilityConfigPanel';
import LegalMatrixModule from './LegalMatrixModule';

type TabType = 'veille' | 'services' | 'eligibility' | 'matrix';

export default function AuditVeillePanel() {
    const [activeTab, setActiveTab] = useState<TabType>('veille');

    // Mock des actualités juridiques pour la veille
    const legalUpdates = [
        {
            id: 1,
            date: '17 Janvier 2026',
            title: 'Nouveau décret sur le Passeport Talent (Salarie qualifié)',
            summary: 'Le seuil de rémunération annuelle a été relevé. Vérifiez la mise à jour des seuils financiers.',
            category: 'Immigration Professionnelle',
            severity: 'high'
        },
        {
            id: 2,
            date: '15 Janvier 2026',
            title: 'Simplification du regroupement familial (Algériens)',
            summary: 'Assouplissement des critères de logement pour les dossiers déposés en IDF.',
            category: 'Regroupement Familial',
            severity: 'medium'
        },
        {
            id: 3,
            date: '10 Janvier 2026',
            title: 'Jurisprudence : Notion de communauté de vie',
            summary: 'Nouvelle décision du Conseil d\'État sur la preuve de cohabitation continue.',
            category: 'VPF',
            severity: 'low'
        }
    ];

    const renderVeilleJuridique = () => (
        <div className="p-6 space-y-8 animate-in fade-in slide-in-from-bottom-4 duration-500">
            {/* Statistiques rapides */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                <div className="bg-white p-6 rounded-3xl border border-slate-200 shadow-sm flex items-center gap-4 group hover:border-indigo-500 transition-all">
                    <div className="w-12 h-12 bg-indigo-100 rounded-2xl flex items-center justify-center text-indigo-600 group-hover:scale-110 transition-transform">
                        <History size={24} />
                    </div>
                    <div>
                        <p className="text-xs font-black text-slate-400 uppercase tracking-widest">Dernière mise à jour</p>
                        <p className="text-lg font-black text-slate-900">Il y a 2 heures</p>
                    </div>
                </div>
                <div className="bg-white p-6 rounded-3xl border border-slate-200 shadow-sm flex items-center gap-4 group hover:border-emerald-500 transition-all">
                    <div className="w-12 h-12 bg-emerald-100 rounded-2xl flex items-center justify-center text-emerald-600 group-hover:scale-110 transition-transform">
                        <Scale size={24} />
                    </div>
                    <div>
                        <p className="text-xs font-black text-slate-400 uppercase tracking-widest">Conformité Specs</p>
                        <p className="text-lg font-black text-slate-900">100% à jour</p>
                    </div>
                </div>
                <div className="bg-white p-6 rounded-3xl border border-slate-200 shadow-sm flex items-center gap-4 group hover:border-amber-500 transition-all">
                    <div className="w-12 h-12 bg-amber-100 rounded-2xl flex items-center justify-center text-amber-600 group-hover:scale-110 transition-transform">
                        <AlertTriangle size={24} />
                    </div>
                    <div>
                        <p className="text-xs font-black text-slate-400 uppercase tracking-widest">Alertes de veille</p>
                        <p className="text-lg font-black text-slate-900">3 nouvelles</p>
                    </div>
                </div>
            </div>

            {/* Flux de veille */}
            <div className="space-y-4">
                <div className="flex items-center justify-between">
                    <h2 className="text-xl font-black text-slate-900 flex items-center gap-2">
                        <Eye className="text-indigo-600" size={24} /> Flux de Veille Juridique
                    </h2>
                    <button className="text-sm font-bold text-indigo-600 hover:underline">Voir toutes les archives</button>
                </div>

                <div className="space-y-4">
                    {legalUpdates.map((update) => (
                        <div key={update.id} className="bg-white rounded-3xl border border-slate-200 p-6 flex gap-4 hover:shadow-xl hover:shadow-indigo-500/5 transition-all group">
                            <div className={`w-1 font-black rounded-full ${update.severity === 'high' ? 'bg-red-500' :
                                update.severity === 'medium' ? 'bg-amber-500' : 'bg-blue-500'
                                }`} />
                            <div className="flex-1 space-y-2">
                                <div className="flex items-center justify-between">
                                    <span className="text-[10px] font-black uppercase tracking-widest px-2 py-1 bg-slate-100 text-slate-500 rounded-lg">
                                        {update.category}
                                    </span>
                                    <span className="text-xs font-bold text-slate-400">{update.date}</span>
                                </div>
                                <h3 className="font-black text-slate-900 group-hover:text-indigo-600 transition-colors uppercase tracking-tight">
                                    {update.title}
                                </h3>
                                <p className="text-sm text-slate-500 leading-relaxed font-medium">
                                    {update.summary}
                                </p>
                                <div className="flex gap-2 pt-2">
                                    <button className="flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-indigo-600 hover:text-white transition-all">
                                        <FileText size={14} /> Consulter le texte de loi
                                    </button>
                                    <button
                                        onClick={() => setActiveTab(update.title.includes('seuil') ? 'eligibility' : 'services')}
                                        className="flex items-center gap-1 px-3 py-1.5 bg-slate-50 border border-slate-100 rounded-xl text-xs font-bold text-slate-600 hover:bg-emerald-600 hover:text-white transition-all"
                                    >
                                        <Zap size={14} /> Appliquer la mise à jour
                                    </button>
                                </div>
                            </div>
                        </div>
                    ))}
                </div>
            </div>

            {/* Outils juristes */}
            <div className="bg-slate-900 rounded-[2.5rem] p-10 text-white relative overflow-hidden group">
                <div className="absolute top-0 right-0 w-64 h-64 bg-indigo-600 rounded-full blur-[100px] opacity-20 -mr-32 -mt-32 group-hover:opacity-40 transition-opacity"></div>
                <div className="relative z-10 space-y-6">
                    <h2 className="text-2xl font-black uppercase tracking-tighter">Outils de Publication</h2>
                    <p className="text-slate-400 font-medium max-w-xl">
                        En tant que juriste référent, vous pouvez publier des notes de veille qui apparaîtront dans les dashboards des agences locales.
                    </p>
                    <button className="px-8 py-4 bg-white text-slate-900 rounded-2xl font-black shadow-2xl hover:bg-slate-100 transition-all active:scale-95">
                        Rédiger une note de veille
                    </button>
                </div>
            </div>
        </div>
    );

    return (
        <div className="h-full flex flex-col bg-slate-50 overflow-hidden">
            {/* Header / Sub-nav */}
            <div className="bg-white border-b border-slate-200 px-8 pt-6">
                <div className="flex items-center justify-between mb-6">
                    <div>
                        <h1 className="text-2xl font-black text-slate-900 tracking-tight flex items-center gap-2">
                            <Scale className="text-indigo-600" size={28} /> Audit et Veille Juridique
                        </h1>
                        <p className="text-slate-500 font-medium text-sm">Gérez la conformité et suivez les évolutions législatives.</p>
                    </div>
                </div>

                <div className="flex gap-8">
                    {[
                        { id: 'veille', label: '📊 Veille Juridique', icon: <Eye size={18} /> },
                        { id: 'matrix', label: '🧠 Matrice IA', icon: <Brain size={18} /> },
                        { id: 'services', label: '⚙️ Services & Documents', icon: <Settings size={18} /> },
                        { id: 'eligibility', label: '🎓 Éligibilité No-Code', icon: <Zap size={18} /> }
                    ].map((tab) => (
                        <button
                            key={tab.id}
                            onClick={() => setActiveTab(tab.id as TabType)}
                            className={`flex items-center gap-2 px-1 py-4 text-sm font-black uppercase tracking-widest border-b-2 transition-all ${activeTab === tab.id
                                    ? 'border-indigo-600 text-indigo-600'
                                    : 'border-transparent text-slate-400 hover:text-slate-600 hover:border-slate-300'
                                }`}
                        >
                            {tab.icon}
                            {tab.label}
                        </button>
                    ))}
                </div>
            </div>

            {/* Content Area */}
            <div className="flex-1 overflow-auto">
                {activeTab === 'veille' && renderVeilleJuridique()}
                {activeTab === 'matrix' && <LegalMatrixModule />}
                {activeTab === 'services' && <ServiceConfigPanel />}
                {activeTab === 'eligibility' && <EligibilityConfigPanel />}
            </div>
        </div>
    );
}
