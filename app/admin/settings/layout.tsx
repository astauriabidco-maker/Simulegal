'use client';

import React from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { Building2, CreditCard, DollarSign, FileStack, Bell, Cpu, ArrowLeft, LayoutGrid, ShieldAlert, FileText, Zap } from 'lucide-react';

const MENU_GROUPS = [
    {
        title: 'Identité & Légal',
        items: [
            { label: 'Infos Société', href: '/admin/settings/general', icon: Building2 },
            { label: 'CGV & Contrats', href: '/admin/settings/legal', icon: FileText }
        ]
    },
    {
        title: 'Finance & Offres',
        items: [
            { label: 'Paiements Stripe', href: '/admin/settings/billing', icon: CreditCard },
            { label: 'Tarifs & Services', href: '/admin/settings/pricing', icon: DollarSign },
        ]
    },
    {
        title: 'Communication',
        items: [
            { label: 'Emails & WhatsApp', href: '/admin/settings/notifications', icon: Bell },
        ]
    },
    {
        title: 'Système & IT',
        items: [
            { label: 'Catalogue Pièces', href: '/admin/settings/catalog', icon: FileStack },
            { label: 'Avancé', href: '/admin/settings/advanced', icon: Cpu }
        ]
    }
];

import RoleGuard from '../../../components/auth/RoleGuard';

export default function SettingsLayout({ children }: { children: React.ReactNode }) {
    const pathname = usePathname();

    return (
        <RoleGuard allowedRoles={['SUPERADMIN']}>
            <div className="p-8">
                <div className="flex justify-between items-center mb-8">
                    <div>
                        <h1 className="text-3xl font-black text-slate-900 tracking-tight">Paramètres</h1>
                        <p className="text-slate-500 font-medium mt-1">Gérez l'ensemble de l'infrastructure et de l'identité système.</p>
                    </div>
                    <div className="flex items-center gap-2 px-3 py-1.5 bg-indigo-50 rounded-full border border-indigo-100 shadow-sm">
                        <ShieldAlert size={14} className="text-indigo-600" />
                        <span className="text-[10px] font-black uppercase text-indigo-700 tracking-widest">Super-Admin Access</span>
                    </div>
                </div>

                <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
                    {/* VERTICAL FIXED SIDEBAR */}
                    <div className="lg:col-span-1 space-y-6">
                        {MENU_GROUPS.map((group, i) => (
                            <div key={i} className="space-y-2">
                                <h3 className="px-4 text-[10px] font-black uppercase text-slate-400 tracking-[0.2em]">
                                    {group.title}
                                </h3>
                                <div className="space-y-1">
                                    {group.items.map((item, j) => {
                                        const isActive = pathname === item.href;
                                        return (
                                            <Link
                                                key={j}
                                                href={item.href}
                                                className={`w-full flex items-center justify-between px-4 py-3 rounded-2xl font-bold text-sm transition-all border-2 group ${isActive
                                                    ? 'bg-white border-slate-200 shadow-sm text-indigo-600'
                                                    : 'border-transparent text-slate-500 hover:bg-slate-200/50 hover:text-slate-700'
                                                    }`}
                                            >
                                                <span className="flex items-center gap-3">
                                                    <item.icon size={16} className={`${isActive ? 'text-indigo-600' : 'text-slate-400 group-hover:text-slate-500'} transition-colors`} />
                                                    {item.label}
                                                </span>
                                            </Link>
                                        );
                                    })}
                                </div>
                            </div>
                        ))}
                    </div>

                    {/* DYNAMIC CONTENT AREA */}
                    <div className="lg:col-span-3">
                        <div className="bg-white rounded-[2rem] p-8 border border-slate-100 shadow-sm min-h-[500px]">
                            {children}
                        </div>
                    </div>
                </div>
            </div>
        </RoleGuard>
    );
}
