'use client';

import React, { useState, useEffect, useCallback } from 'react';

// ═══════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════

interface AutomationRule {
    id: string;
    name: string;
    description: string;
    type: 'RELANCE' | 'ESCALADE' | 'AUTO_TRANSITION' | 'NOTIFICATION' | 'ALERTE_INTERNE';
    enabled: boolean;
    priority: number;
    trigger: {
        event: 'STAGE_CHANGE' | 'STALE_DAYS' | 'DOCS_COMPLETE' | 'PAYMENT_RECEIVED' | 'MANUAL';
        stage?: string;
        fromStage?: string;
        toStage?: string;
        staleDays?: number;
        pipelineTemplates?: string[];
        categories?: string[];
    };
    action: {
        type: 'WHATSAPP' | 'EMAIL' | 'MOVE_STAGE' | 'INTERNAL_ALERT' | 'ASSIGN_USER';
        template?: string;
        message?: string;
        targetStage?: string;
        alertLevel?: 'INFO' | 'WARNING' | 'CRITICAL';
        assignTo?: 'MANAGER' | 'ADMIN' | 'SPECIFIC_USER';
        maxExecutions?: number;
        cooldownHours?: number;
    };
    createdAt: string;
    updatedAt: string;
    executionCount?: number;
}

interface AutomationLog {
    id: string;
    ruleId: string;
    ruleName: string;
    leadId: string;
    leadName: string;
    action: string;
    result: 'SUCCESS' | 'FAILED' | 'SKIPPED';
    details?: string;
    executedAt: string;
}

// ═══════════════════════════════════════════════
// CONSTANTS
// ═══════════════════════════════════════════════

const API = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5000';

const TYPE_LABELS: Record<string, { label: string; icon: string; color: string }> = {
    NOTIFICATION: { label: 'Notification', icon: '🔔', color: '#3b82f6' },
    RELANCE: { label: 'Relance', icon: '🔄', color: '#f59e0b' },
    ESCALADE: { label: 'Escalade', icon: '⬆️', color: '#ef4444' },
    AUTO_TRANSITION: { label: 'Auto-transition', icon: '⚡', color: '#10b981' },
    ALERTE_INTERNE: { label: 'Alerte interne', icon: '📢', color: '#8b5cf6' },
};

const EVENT_LABELS: Record<string, string> = {
    STAGE_CHANGE: 'Changement d\'étape',
    STALE_DAYS: 'Inactivité (jours)',
    DOCS_COMPLETE: 'Documents complets',
    PAYMENT_RECEIVED: 'Paiement reçu',
    MANUAL: 'Manuel',
};

const ACTION_LABELS: Record<string, string> = {
    WHATSAPP: '💬 WhatsApp',
    EMAIL: '📧 Email',
    MOVE_STAGE: '➡️ Déplacer étape',
    INTERNAL_ALERT: '🚨 Alerte interne',
    ASSIGN_USER: '👤 Assigner',
};

const STAGE_OPTIONS = [
    { value: '*', label: 'Toutes les étapes' },
    { value: 'NEW', label: 'Nouveau' },
    { value: 'PAID', label: 'Payé' },
    { value: 'COLLECTING', label: 'Collecte documents' },
    { value: 'REVIEW', label: 'Vérification juriste' },
    { value: 'HUNTING', label: 'Hunting RDV' },
    { value: 'BOOKED', label: 'RDV réservé' },
    { value: 'ANTS_DEPOSIT', label: 'Dépôt ANTS' },
    { value: 'ORIGINALS_WAIT', label: 'Originaux attendus' },
    { value: 'OFII_INVESTIGATION', label: 'Enquête OFII' },
    { value: 'INSTRUCTION', label: 'Instruction' },
    { value: 'DECISION_WAIT', label: 'Attente décision' },
    { value: 'DONE', label: 'Terminé' },
    { value: 'CALLBACK', label: 'À rappeler' },
];

// ═══════════════════════════════════════════════
// COMPONENT
// ═══════════════════════════════════════════════

export default function PipelineAutomationsTab() {
    const [rules, setRules] = useState<AutomationRule[]>([]);
    const [logs, setLogs] = useState<AutomationLog[]>([]);
    const [stats, setStats] = useState<any>(null);
    const [loading, setLoading] = useState(true);
    const [tab, setTab] = useState<'rules' | 'logs' | 'create'>('rules');
    const [editingRule, setEditingRule] = useState<AutomationRule | null>(null);
    const [saving, setSaving] = useState(false);

    // ── Form state for create/edit ──
    const [formData, setFormData] = useState<Partial<AutomationRule>>({
        name: '',
        description: '',
        type: 'NOTIFICATION',
        enabled: true,
        priority: 3,
        trigger: { event: 'STAGE_CHANGE' },
        action: { type: 'WHATSAPP', message: '', maxExecutions: 1, cooldownHours: 0 },
    });

    const fetchRules = useCallback(async () => {
        try {
            const [rulesRes, statsRes, logsRes] = await Promise.all([
                fetch(`${API}/pipeline-automations/rules`),
                fetch(`${API}/pipeline-automations/stats`),
                fetch(`${API}/pipeline-automations/logs?limit=50`),
            ]);
            const rulesData = await rulesRes.json();
            const statsData = await statsRes.json();
            const logsData = await logsRes.json();
            setRules(rulesData.rules || []);
            setStats(statsData);
            setLogs(logsData.logs || []);
        } catch (e) {
            console.error('Failed to fetch automations', e);
        } finally {
            setLoading(false);
        }
    }, []);

    useEffect(() => { fetchRules(); }, [fetchRules]);

    const toggleRule = async (id: string) => {
        await fetch(`${API}/pipeline-automations/rules/${id}/toggle`, { method: 'POST' });
        fetchRules();
    };

    const deleteRule = async (id: string) => {
        if (!confirm('Supprimer cette automatisation ?')) return;
        await fetch(`${API}/pipeline-automations/rules/${id}`, { method: 'DELETE' });
        fetchRules();
    };

    const saveRule = async () => {
        setSaving(true);
        try {
            const url = editingRule
                ? `${API}/pipeline-automations/rules/${editingRule.id}`
                : `${API}/pipeline-automations/rules`;
            const method = editingRule ? 'PUT' : 'POST';
            await fetch(url, {
                method,
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(formData),
            });
            setEditingRule(null);
            setTab('rules');
            resetForm();
            fetchRules();
        } finally {
            setSaving(false);
        }
    };

    const startEdit = (rule: AutomationRule) => {
        setEditingRule(rule);
        setFormData({ ...rule });
        setTab('create');
    };

    const resetForm = () => {
        setFormData({
            name: '', description: '', type: 'NOTIFICATION', enabled: true, priority: 3,
            trigger: { event: 'STAGE_CHANGE' },
            action: { type: 'WHATSAPP', message: '', maxExecutions: 1, cooldownHours: 0 },
        });
        setEditingRule(null);
    };

    const runStaleCheck = async () => {
        const res = await fetch(`${API}/pipeline-automations/check-stale`, { method: 'POST' });
        const data = await res.json();
        alert(`Vérification terminée: ${data.processed} dossiers analysés, ${data.actions} actions exécutées`);
        fetchRules();
    };

    if (loading) {
        return (
            <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center', height: 300 }}>
                <div style={{ fontSize: 16, color: '#94a3b8' }}>Chargement des automatisations...</div>
            </div>
        );
    }

    return (
        <div style={{ padding: 0 }}>
            {/* ── STATS HEADER ── */}
            {stats && (
                <div style={{ display: 'grid', gridTemplateColumns: 'repeat(4, 1fr)', gap: 16, marginBottom: 24 }}>
                    <StatCard label="Total règles" value={stats.totalRules} icon="📋" color="#3b82f6" />
                    <StatCard label="Actives" value={stats.activeRules} icon="✅" color="#10b981" />
                    <StatCard label="Exécutions totales" value={stats.totalExecutions} icon="⚡" color="#f59e0b" />
                    <StatCard label="Types" value={Object.keys(stats.byType || {}).length} icon="🏷️" color="#8b5cf6" />
                </div>
            )}

            {/* ── TABS ── */}
            <div style={{ display: 'flex', gap: 8, marginBottom: 20, borderBottom: '1px solid rgba(255,255,255,0.1)', paddingBottom: 12 }}>
                <TabButton active={tab === 'rules'} onClick={() => { setTab('rules'); resetForm(); }}>📋 Règles ({rules.length})</TabButton>
                <TabButton active={tab === 'logs'} onClick={() => setTab('logs')}>📜 Historique</TabButton>
                <TabButton active={tab === 'create'} onClick={() => { resetForm(); setTab('create'); }}>➕ Nouvelle règle</TabButton>
                <div style={{ flex: 1 }} />
                <button
                    onClick={runStaleCheck}
                    style={{
                        padding: '8px 16px', borderRadius: 8, border: '1px solid rgba(59,130,246,0.3)',
                        background: 'rgba(59,130,246,0.1)', color: '#60a5fa', cursor: 'pointer', fontSize: 13,
                    }}
                >
                    🔍 Vérifier maintenant
                </button>
            </div>

            {/* ── RULES LIST ── */}
            {tab === 'rules' && (
                <div style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
                    {rules.map(rule => (
                        <RuleCard
                            key={rule.id}
                            rule={rule}
                            onToggle={() => toggleRule(rule.id)}
                            onEdit={() => startEdit(rule)}
                            onDelete={() => deleteRule(rule.id)}
                        />
                    ))}
                    {rules.length === 0 && (
                        <div style={{ textAlign: 'center', padding: 40, color: '#64748b' }}>
                            Aucune automatisation configurée. Créez-en une !
                        </div>
                    )}
                </div>
            )}

            {/* ── LOGS ── */}
            {tab === 'logs' && (
                <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
                    {logs.map(log => (
                        <LogEntry key={log.id} log={log} />
                    ))}
                    {logs.length === 0 && (
                        <div style={{ textAlign: 'center', padding: 40, color: '#64748b' }}>
                            Aucun historique d'exécution
                        </div>
                    )}
                </div>
            )}

            {/* ── CREATE/EDIT FORM ── */}
            {tab === 'create' && (
                <RuleForm
                    formData={formData}
                    setFormData={setFormData}
                    onSave={saveRule}
                    onCancel={() => { resetForm(); setTab('rules'); }}
                    saving={saving}
                    isEditing={!!editingRule}
                />
            )}
        </div>
    );
}

// ═══════════════════════════════════════════════
// SUB-COMPONENTS
// ═══════════════════════════════════════════════

function StatCard({ label, value, icon, color }: { label: string; value: number; icon: string; color: string }) {
    return (
        <div style={{
            background: `linear-gradient(135deg, ${color}15, ${color}08)`,
            border: `1px solid ${color}30`,
            borderRadius: 12, padding: '16px 20px',
        }}>
            <div style={{ fontSize: 24, marginBottom: 4 }}>{icon}</div>
            <div style={{ fontSize: 28, fontWeight: 700, color }}>{value}</div>
            <div style={{ fontSize: 12, color: '#94a3b8', marginTop: 2 }}>{label}</div>
        </div>
    );
}

function TabButton({ active, onClick, children }: { active: boolean; onClick: () => void; children: React.ReactNode }) {
    return (
        <button
            onClick={onClick}
            style={{
                padding: '8px 16px', borderRadius: 8, border: 'none', cursor: 'pointer',
                background: active ? 'rgba(59,130,246,0.2)' : 'transparent',
                color: active ? '#60a5fa' : '#94a3b8',
                fontWeight: active ? 600 : 400, fontSize: 13,
                transition: 'all 0.2s',
            }}
        >
            {children}
        </button>
    );
}

function RuleCard({ rule, onToggle, onEdit, onDelete }: {
    rule: AutomationRule; onToggle: () => void; onEdit: () => void; onDelete: () => void;
}) {
    const typeInfo = TYPE_LABELS[rule.type] || { label: rule.type, icon: '❓', color: '#666' };
    const eventLabel = EVENT_LABELS[rule.trigger.event] || rule.trigger.event;
    const actionLabel = ACTION_LABELS[rule.action.type] || rule.action.type;

    return (
        <div style={{
            background: rule.enabled ? 'rgba(255,255,255,0.03)' : 'rgba(255,255,255,0.01)',
            border: `1px solid ${rule.enabled ? typeInfo.color + '40' : 'rgba(255,255,255,0.06)'}`,
            borderRadius: 12, padding: '16px 20px',
            opacity: rule.enabled ? 1 : 0.5,
            transition: 'all 0.3s',
        }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: 12, marginBottom: 10 }}>
                <span style={{ fontSize: 22 }}>{typeInfo.icon}</span>
                <div style={{ flex: 1 }}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
                        <span style={{ fontWeight: 600, color: '#e2e8f0', fontSize: 14 }}>{rule.name}</span>
                        <span style={{
                            padding: '2px 8px', borderRadius: 20, fontSize: 10, fontWeight: 600,
                            background: typeInfo.color + '20', color: typeInfo.color,
                        }}>
                            {typeInfo.label}
                        </span>
                        <span style={{
                            padding: '2px 8px', borderRadius: 20, fontSize: 10,
                            background: 'rgba(59,130,246,0.1)', color: '#60a5fa',
                        }}>
                            P{rule.priority}
                        </span>
                    </div>
                    <div style={{ fontSize: 12, color: '#64748b', marginTop: 2 }}>{rule.description}</div>
                </div>
                <div style={{ display: 'flex', gap: 6, alignItems: 'center' }}>
                    <span style={{ fontSize: 11, color: '#64748b' }}>
                        {rule.executionCount || 0} exec
                    </span>
                    <button onClick={onToggle} style={{
                        width: 44, height: 24, borderRadius: 12, border: 'none', cursor: 'pointer',
                        background: rule.enabled ? '#10b981' : '#374151',
                        position: 'relative', transition: 'background 0.3s',
                    }}>
                        <div style={{
                            width: 18, height: 18, borderRadius: '50%', background: '#fff',
                            position: 'absolute', top: 3,
                            left: rule.enabled ? 23 : 3,
                            transition: 'left 0.3s',
                        }} />
                    </button>
                    <button onClick={onEdit} style={{
                        padding: '4px 8px', borderRadius: 6, border: '1px solid rgba(255,255,255,0.1)',
                        background: 'transparent', color: '#94a3b8', cursor: 'pointer', fontSize: 12,
                    }}>✏️</button>
                    <button onClick={onDelete} style={{
                        padding: '4px 8px', borderRadius: 6, border: '1px solid rgba(239,68,68,0.2)',
                        background: 'transparent', color: '#ef4444', cursor: 'pointer', fontSize: 12,
                    }}>🗑️</button>
                </div>
            </div>

            {/* Trigger & Action summary */}
            <div style={{ display: 'flex', gap: 16, fontSize: 12, color: '#94a3b8' }}>
                <div style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
                    <span style={{ color: '#f59e0b' }}>⚡</span>
                    {eventLabel}
                    {rule.trigger.toStage && <span style={{ color: '#60a5fa' }}> → {rule.trigger.toStage}</span>}
                    {rule.trigger.staleDays && <span style={{ color: '#f59e0b' }}> ≥ {rule.trigger.staleDays}j</span>}
                    {rule.trigger.stage && rule.trigger.stage !== '*' && (
                        <span style={{ color: '#a78bfa' }}> @ {rule.trigger.stage}</span>
                    )}
                </div>
                <div style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
                    <span>→</span>
                    {actionLabel}
                    {rule.action.targetStage && <span style={{ color: '#10b981' }}> → {rule.action.targetStage}</span>}
                </div>
                {rule.action.maxExecutions && rule.action.maxExecutions > 0 && (
                    <span style={{ color: '#64748b' }}>max {rule.action.maxExecutions}x</span>
                )}
                {rule.action.cooldownHours && rule.action.cooldownHours > 0 && (
                    <span style={{ color: '#64748b' }}>cooldown {rule.action.cooldownHours}h</span>
                )}
            </div>
        </div>
    );
}

function LogEntry({ log }: { log: AutomationLog }) {
    const isSuccess = log.result === 'SUCCESS';
    return (
        <div style={{
            display: 'flex', alignItems: 'center', gap: 12, padding: '10px 16px',
            background: 'rgba(255,255,255,0.02)', borderRadius: 8,
            border: `1px solid ${isSuccess ? 'rgba(16,185,129,0.2)' : 'rgba(239,68,68,0.2)'}`,
            fontSize: 13,
        }}>
            <span style={{ fontSize: 16 }}>{isSuccess ? '✅' : '❌'}</span>
            <div style={{ flex: 1 }}>
                <span style={{ fontWeight: 500, color: '#e2e8f0' }}>{log.ruleName}</span>
                <span style={{ color: '#64748b' }}> — {log.leadName}</span>
                {log.details && (
                    <div style={{ fontSize: 11, color: '#64748b', marginTop: 2, maxWidth: 500, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                        {log.details}
                    </div>
                )}
            </div>
            <span style={{ fontSize: 11, color: '#64748b' }}>
                {new Date(log.executedAt).toLocaleString('fr-FR')}
            </span>
        </div>
    );
}

function RuleForm({ formData, setFormData, onSave, onCancel, saving, isEditing }: {
    formData: Partial<AutomationRule>;
    setFormData: (data: Partial<AutomationRule>) => void;
    onSave: () => void;
    onCancel: () => void;
    saving: boolean;
    isEditing: boolean;
}) {
    const inputStyle: React.CSSProperties = {
        width: '100%', padding: '10px 14px', borderRadius: 8, border: '1px solid rgba(255,255,255,0.1)',
        background: 'rgba(255,255,255,0.05)', color: '#e2e8f0', fontSize: 13, outline: 'none',
    };
    const selectStyle: React.CSSProperties = { ...inputStyle, appearance: 'none' as const };
    const labelStyle: React.CSSProperties = { display: 'block', fontSize: 12, fontWeight: 600, color: '#94a3b8', marginBottom: 6 };

    const updateTrigger = (key: string, value: any) => {
        setFormData({ ...formData, trigger: { ...formData.trigger!, [key]: value } });
    };
    const updateAction = (key: string, value: any) => {
        setFormData({ ...formData, action: { ...formData.action!, [key]: value } });
    };

    return (
        <div style={{ display: 'flex', flexDirection: 'column', gap: 20 }}>
            <h3 style={{ color: '#e2e8f0', fontSize: 16, margin: 0 }}>
                {isEditing ? '✏️ Modifier l\'automatisation' : '➕ Nouvelle automatisation'}
            </h3>

            {/* ── INFOS GÉNÉRALES ── */}
            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 16 }}>
                <div>
                    <label style={labelStyle}>Nom</label>
                    <input style={inputStyle} value={formData.name || ''} placeholder="Nom de l'automatisation"
                        onChange={e => setFormData({ ...formData, name: e.target.value })} />
                </div>
                <div>
                    <label style={labelStyle}>Type</label>
                    <select style={selectStyle} value={formData.type || 'NOTIFICATION'}
                        onChange={e => setFormData({ ...formData, type: e.target.value as any })}>
                        {Object.entries(TYPE_LABELS).map(([k, v]) => (
                            <option key={k} value={k}>{v.icon} {v.label}</option>
                        ))}
                    </select>
                </div>
            </div>

            <div>
                <label style={labelStyle}>Description</label>
                <input style={inputStyle} value={formData.description || ''} placeholder="Description..."
                    onChange={e => setFormData({ ...formData, description: e.target.value })} />
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 16 }}>
                <div>
                    <label style={labelStyle}>Priorité (1=haute, 5=basse)</label>
                    <input style={inputStyle} type="number" min={1} max={5} value={formData.priority || 3}
                        onChange={e => setFormData({ ...formData, priority: parseInt(e.target.value) })} />
                </div>
                <div style={{ display: 'flex', alignItems: 'center', gap: 8, paddingTop: 20 }}>
                    <input type="checkbox" checked={formData.enabled ?? true}
                        onChange={e => setFormData({ ...formData, enabled: e.target.checked })} />
                    <span style={{ color: '#94a3b8', fontSize: 13 }}>Activée</span>
                </div>
            </div>

            {/* ── TRIGGER ── */}
            <div style={{ background: 'rgba(245,158,11,0.05)', border: '1px solid rgba(245,158,11,0.2)', borderRadius: 12, padding: 16 }}>
                <h4 style={{ color: '#f59e0b', margin: '0 0 12px', fontSize: 14 }}>⚡ Déclencheur</h4>
                <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12 }}>
                    <div>
                        <label style={labelStyle}>Événement</label>
                        <select style={selectStyle} value={formData.trigger?.event || 'STAGE_CHANGE'}
                            onChange={e => updateTrigger('event', e.target.value)}>
                            {Object.entries(EVENT_LABELS).map(([k, v]) => (
                                <option key={k} value={k}>{v}</option>
                            ))}
                        </select>
                    </div>

                    {formData.trigger?.event === 'STAGE_CHANGE' && (
                        <div>
                            <label style={labelStyle}>Vers étape</label>
                            <select style={selectStyle} value={formData.trigger?.toStage || ''}
                                onChange={e => updateTrigger('toStage', e.target.value)}>
                                <option value="">Toutes</option>
                                {STAGE_OPTIONS.map(s => (
                                    <option key={s.value} value={s.value}>{s.label}</option>
                                ))}
                            </select>
                        </div>
                    )}

                    {formData.trigger?.event === 'STALE_DAYS' && (
                        <>
                            <div>
                                <label style={labelStyle}>Jours d'inactivité</label>
                                <input style={inputStyle} type="number" min={1} value={formData.trigger?.staleDays || 7}
                                    onChange={e => updateTrigger('staleDays', parseInt(e.target.value))} />
                            </div>
                            <div>
                                <label style={labelStyle}>Étape surveillée</label>
                                <select style={selectStyle} value={formData.trigger?.stage || '*'}
                                    onChange={e => updateTrigger('stage', e.target.value)}>
                                    {STAGE_OPTIONS.map(s => (
                                        <option key={s.value} value={s.value}>{s.label}</option>
                                    ))}
                                </select>
                            </div>
                        </>
                    )}
                </div>
            </div>

            {/* ── ACTION ── */}
            <div style={{ background: 'rgba(16,185,129,0.05)', border: '1px solid rgba(16,185,129,0.2)', borderRadius: 12, padding: 16 }}>
                <h4 style={{ color: '#10b981', margin: '0 0 12px', fontSize: 14 }}>🎯 Action</h4>
                <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12 }}>
                    <div>
                        <label style={labelStyle}>Type d'action</label>
                        <select style={selectStyle} value={formData.action?.type || 'WHATSAPP'}
                            onChange={e => updateAction('type', e.target.value)}>
                            {Object.entries(ACTION_LABELS).map(([k, v]) => (
                                <option key={k} value={k}>{v}</option>
                            ))}
                        </select>
                    </div>

                    {formData.action?.type === 'MOVE_STAGE' && (
                        <div>
                            <label style={labelStyle}>Étape cible</label>
                            <select style={selectStyle} value={formData.action?.targetStage || ''}
                                onChange={e => updateAction('targetStage', e.target.value)}>
                                {STAGE_OPTIONS.filter(s => s.value !== '*').map(s => (
                                    <option key={s.value} value={s.value}>{s.label}</option>
                                ))}
                            </select>
                        </div>
                    )}

                    {formData.action?.type === 'INTERNAL_ALERT' && (
                        <div>
                            <label style={labelStyle}>Niveau</label>
                            <select style={selectStyle} value={formData.action?.alertLevel || 'INFO'}
                                onChange={e => updateAction('alertLevel', e.target.value)}>
                                <option value="INFO">ℹ️ Info</option>
                                <option value="WARNING">⚠️ Warning</option>
                                <option value="CRITICAL">🔴 Critical</option>
                            </select>
                        </div>
                    )}
                </div>

                <div style={{ marginTop: 12 }}>
                    <label style={labelStyle}>
                        Message (variables: {'{{name}}, {{stage}}, {{days}}, {{service}}, {{email}}'})
                    </label>
                    <textarea
                        style={{ ...inputStyle, height: 80, resize: 'vertical' }}
                        value={formData.action?.message || ''}
                        placeholder="{{name}}, votre dossier {{service}} nécessite votre attention..."
                        onChange={e => updateAction('message', e.target.value)}
                    />
                </div>

                <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12, marginTop: 12 }}>
                    <div>
                        <label style={labelStyle}>Max exécutions (0 = illimité)</label>
                        <input style={inputStyle} type="number" min={0} value={formData.action?.maxExecutions || 0}
                            onChange={e => updateAction('maxExecutions', parseInt(e.target.value))} />
                    </div>
                    <div>
                        <label style={labelStyle}>Cooldown (heures)</label>
                        <input style={inputStyle} type="number" min={0} value={formData.action?.cooldownHours || 0}
                            onChange={e => updateAction('cooldownHours', parseInt(e.target.value))} />
                    </div>
                </div>
            </div>

            {/* ── BUTTONS ── */}
            <div style={{ display: 'flex', gap: 12, justifyContent: 'flex-end' }}>
                <button onClick={onCancel} style={{
                    padding: '10px 20px', borderRadius: 8, border: '1px solid rgba(255,255,255,0.1)',
                    background: 'transparent', color: '#94a3b8', cursor: 'pointer', fontSize: 13,
                }}>
                    Annuler
                </button>
                <button onClick={onSave} disabled={saving} style={{
                    padding: '10px 24px', borderRadius: 8, border: 'none',
                    background: 'linear-gradient(135deg, #3b82f6, #2563eb)',
                    color: '#fff', cursor: 'pointer', fontWeight: 600, fontSize: 13,
                    opacity: saving ? 0.6 : 1,
                }}>
                    {saving ? '⏳ Sauvegarde...' : (isEditing ? '✅ Mettre à jour' : '✅ Créer')}
                </button>
            </div>
        </div>
    );
}
