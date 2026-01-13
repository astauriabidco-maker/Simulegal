'use client';

import React, { useMemo } from 'react';
import { UserProfile, ProcedureRule } from '@/types';
import { evaluateRule } from '@/lib/engine';
import rulesSejour from '@/specs/rules_sejour.json';
import rulesNaturalisation from '@/specs/rules_naturalisation.json';
import rulesFamily from '@/specs/rules_family.json';
import { ArrowRight, Bell, Scale, CheckCircle2, AlertTriangle, AlertCircle, FileText, Smartphone, Car, Info, XCircle, MapPin, Languages, GraduationCap, Phone, Clock } from 'lucide-react';

import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface ResultsViewProps {
    userProfile: UserProfile;
    onReset: () => void;
    serviceId?: string;
}

export default function ResultsView({ userProfile, onReset, serviceId }: ResultsViewProps) {
    const [showLeadModal, setShowLeadModal] = React.useState(false);
    const [leadForm, setLeadForm] = React.useState({ name: '', phone: '' });
    const isFamilyReunification = serviceId === 'family_reunification';
    const isDrivingExchange = serviceId === 'permis_conduire';
    const isRdvPrefecture = serviceId === 'rdv_prefecture';
    const isLegalConsultation = serviceId === 'rdv_juriste';
    const isFrenchCourse = serviceId === 'french_course';
    const isCivicExam = serviceId === 'examen_civique';
    const isCallback = serviceId === 'rappel_echeances';


    const getDrivingResult = () => {
        const { status, license_country, residence_start_date } = userProfile.driving;

        if (status === 'STUDENT') return 'STUDENT_NO_NEED';
        if (status === 'TOURIST') return 'TOURIST_IMPOSSIBLE';
        if (license_country === 'NO_ACCORD') return 'NO_ACCORD_IMPOSSIBLE';

        if (!residence_start_date) return 'UNKNOWN';

        // Calculate delay
        const [month, year] = residence_start_date.split('/').map(Number);
        const startDate = new Date(year, month - 1, 1);
        const today = new Date('2026-01-13'); // Using system time

        const diffMonths = (today.getFullYear() - startDate.getFullYear()) * 12 + (today.getMonth() - startDate.getMonth());

        if (diffMonths > 12) return 'DELAY_EXCEEDED';
        return 'SUCCESS';
    };

    const familyResults = useMemo(() => {
        if (!isFamilyReunification) return [];
        return (rulesFamily as ProcedureRule[])
            .filter((rule) => evaluateRule(userProfile, rule.conditions));
    }, [userProfile, isFamilyReunification]);

    const eligibleStays = useMemo(() => {
        if (isFamilyReunification) return [];
        let results = (rulesSejour as ProcedureRule[])
            .filter((rule) => evaluateRule(userProfile, rule.conditions));

        if (userProfile.admin.current_visa_type === 'RESIDENT_CARD') {
            const allowedResidencyIds = ['carte_resident_longue_duree_ue', 'carte_resident_refugie_apatride', 'carte_resident_conjoint_francais', 'carte_resident_regroupement_familial'];
            results = results.filter(r => allowedResidencyIds.includes(r.id));
        }

        results.sort((a, b) => b.priority - a.priority);

        const hasStrongOption = results.some(r => r.priority >= 60);
        if (hasStrongOption) {
            results = results.filter(r => r.priority >= 20);
        }

        return results;
    }, [userProfile, isFamilyReunification]);

    const eligibleNaturalization = useMemo(() => {
        if (isFamilyReunification) return [];
        return (rulesNaturalisation as ProcedureRule[])
            .filter((rule) => evaluateRule(userProfile, rule.conditions))
            .sort((a, b) => b.priority - a.priority);
    }, [userProfile, isFamilyReunification]);

    const targetGoal = userProfile.project.target_goal || 'BOTH';
    const hasNatEligible = eligibleNaturalization.length > 0;
    const hasStayEligible = eligibleStays.length > 0;
    const isFamilyEligible = isFamilyReunification && familyResults.length > 0;

    const getFamilyFailureReason = () => {
        const familyRules = rulesFamily as ProcedureRule[];
        const rule = familyRules[0];
        const conditions = rule.conditions.AND || [];

        const durationCondition = conditions[0];
        const resourceConditionGroup = conditions[1];
        const housingCondition = conditions[2];

        if (!evaluateRule(userProfile, durationCondition)) return 'DURATION';

        // Detailed Resource Diagnostics
        if (userProfile.family.has_handicap_allowance === false) {
            if (userProfile.family.income_source === 'RSA_ALOWANCE') return 'SOURCE_RSA';
            if ((userProfile.financial.resources_monthly_average || 0) < 1398) return 'AMOUNT_LOW';
        } else if (userProfile.family.has_handicap_allowance === undefined) {
            // Fallback to general resource check if not captured by specific sub-steps
            if (!evaluateRule(userProfile, resourceConditionGroup)) return 'RESOURCES_GENERAL';
        }

        // Housing Diagnostics
        const housingConditions = conditions.slice(2);
        for (const cond of housingConditions) {
            if (!evaluateRule(userProfile, cond)) return 'HOUSING';
        }

        return 'UNKNOWN';
    };

    const showNaturalizationFirst = targetGoal === 'NATURALIZATION' || targetGoal === 'BOTH';

    const handleProcedureDetails = (currentVisa: string, resultId: string) => {
        if (resultId.startsWith('nat_')) {
            return { label: 'Acquisition de la Nationalité', color: 'bg-blue-600 text-white' };
        }
        if (currentVisa === 'NONE' || currentVisa === 'VISITOR') {
            return { label: 'Première Demande / Régularisation', color: 'bg-orange-500 text-white' };
        }
        if (currentVisa === 'STUDENT' && (resultId.includes('salarie') || resultId.includes('talent') || resultId.includes('rece'))) {
            return { label: 'Changement de Statut', color: 'bg-purple-600 text-white' };
        }
        if (resultId.includes('resident') && currentVisa !== 'RESIDENT_CARD') {
            return { label: 'Accès à la Résidence (10 ans)', color: 'bg-amber-500 text-white' };
        }

        const isVPF = currentVisa === 'VPF' && resultId.includes('vpf');
        const isWorker = (currentVisa === 'WORKER' || currentVisa === 'VLS-TS') && (resultId.includes('salarie') || resultId.includes('temporaire'));
        const isStudent = currentVisa === 'STUDENT' && resultId.includes('etudiant');
        const isTalent = currentVisa === 'PASSEPORT_TALENT' && resultId.includes('talent');
        const isResident = currentVisa === 'RESIDENT_CARD' && resultId.includes('resident');

        if (isVPF || isWorker || isStudent || isTalent || isResident) {
            return { label: 'Renouvellement', color: 'bg-emerald-500 text-white' };
        }

        return { label: 'Nouvelle Procédure', color: 'bg-slate-500 text-white' };
    };

    const handleCheckout = (amount: number, label: string) => {
        // @ts-ignore
        if (window.openAuthAndPaymentFlow) {
            // @ts-ignore
            window.openAuthAndPaymentFlow(amount);
        } else {
            alert(`Simulation : Commande de "${label}" pour ${amount}€`);
        }
    };

    return (
        <div className="p-8 flex-1 flex flex-col">
            <div className="flex justify-between items-center mb-12">
                <div className="space-y-1">
                    <h2 className="text-4xl font-black text-slate-900 tracking-tight">Analyse terminée</h2>
                    <p className="text-slate-500 font-bold uppercase tracking-widest text-xs">Simulégal Intelligence Engine</p>
                </div>
                <button
                    onClick={onReset}
                    className="px-6 py-3 bg-slate-100 text-slate-600 font-bold rounded-xl hover:bg-slate-200 transition-all"
                >
                    Recommencer
                </button>
            </div>

            {/* FAMILY REUNIFICATION SPECIFIC VIEW */}
            {isFamilyReunification && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    {isFamilyEligible ? (
                        <div className="p-12 bg-gradient-to-br from-indigo-600 to-blue-700 rounded-[3rem] text-white shadow-2xl shadow-indigo-200 relative overflow-hidden border-4 border-white">
                            <div className="absolute top-0 right-10 p-12 opacity-10">
                                <CheckCircle2 className="w-64 h-64" />
                            </div>
                            <div className="relative z-10 space-y-8">
                                <div className="inline-flex px-4 py-2 bg-emerald-400 text-emerald-950 rounded-full text-[10px] font-black uppercase tracking-widest">Éligible</div>
                                <h3 className="text-5xl font-black leading-tight max-w-2xl">Vous remplissez les conditions principales !</h3>
                                <p className="text-xl text-indigo-100 font-medium opacity-90 max-w-xl leading-relaxed">
                                    Vos revenus et votre durée de séjour sont conformes. Prochaine étape : préparer votre dossier complet.
                                </p>

                                <div className="pt-8 flex flex-col sm:flex-row gap-6 items-center">
                                    <button
                                        onClick={() => handleCheckout(499, "Accompagnement Regroupement Familial")}
                                        className="group px-10 py-6 bg-white text-indigo-600 text-xl font-black rounded-2xl hover:bg-slate-50 transition-all flex items-center gap-3 shadow-xl"
                                    >
                                        Commander : Accompagnement Dossier
                                        <ArrowRight className="w-6 h-6 group-hover:translate-x-1 transition-transform" />
                                    </button>
                                    <div className="text-indigo-200 font-bold text-sm text-center sm:text-left">
                                        Audit de surface logement &<br />Checklist personnalisée
                                    </div>
                                </div>
                            </div>
                        </div>
                    ) : (
                        <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-xl relative overflow-hidden">
                            <div className="flex flex-col md:flex-row gap-12 items-center">
                                <div className="w-32 h-32 bg-rose-50 text-rose-500 rounded-[2.5rem] flex items-center justify-center shrink-0">
                                    <AlertTriangle className="w-16 h-16" />
                                </div>
                                <div className="flex-1 space-y-6">
                                    {getFamilyFailureReason() === 'DURATION' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Délai de résidence insuffisant</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">Il faut 18 mois de présence (ou 12 mois pour les Algériens). Vous y êtes presque !</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-blue-600 text-white font-black rounded-2xl hover:bg-blue-700 transition-all shadow-lg shadow-blue-100">
                                                <Bell className="w-6 h-6" />
                                                Mettre un rappel d'échéance
                                            </button>
                                        </div>
                                    )}

                                    {getFamilyFailureReason() === 'SOURCE_RSA' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Le RSA ne compte pas</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">Les revenus doivent provenir du travail ou de pensions. L'administration ne prend pas en compte les aides sociales.</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-indigo-600 text-white font-black rounded-2xl hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-100">
                                                <Scale className="w-6 h-6" />
                                                Prendre RDV avec un Juriste
                                            </button>
                                            <p className="text-sm text-slate-400 font-bold border-l-4 border-indigo-100 pl-4 py-1 italic uppercase tracking-tighter">Votre conjoint peut peut-être compléter les revenus.</p>
                                        </div>
                                    )}

                                    {getFamilyFailureReason() === 'AMOUNT_LOW' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Revenus insuffisants</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">Le montant minimum requis est de ~1400€ nets (Moyenne du SMIC sur 12 mois).</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-slate-900 text-white font-black rounded-2xl hover:bg-slate-800 transition-all shadow-lg">
                                                <Scale className="w-6 h-6" />
                                                Étudier mon dossier avec un juriste
                                            </button>
                                            <p className="text-xs text-slate-400 font-bold uppercase tracking-[0.1em]">L'intérêt supérieur de l'enfant peut parfois justifier une dérogation.</p>
                                        </div>
                                    )}

                                    {getFamilyFailureReason() === 'HOUSING' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Logement non disponible</h3>
                                            <p className="text-xl text-slate-500 font-medium">Vous devez disposer d'un logement ou d'une attestation de réservation conforme au CESEDA.</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-slate-600 text-white font-black rounded-2xl hover:bg-slate-700 transition-all">
                                                <FileText className="w-6 h-6" />
                                                Consulter le Guide Logement
                                            </button>
                                        </div>
                                    )}

                                    {(getFamilyFailureReason() === 'RESOURCES_GENERAL' || getFamilyFailureReason() === 'UNKNOWN') && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Dossier Incomplet</h3>
                                            <p className="text-xl text-slate-500 font-medium">Certaines conditions de ressources ou de logement ne semblent pas remplies pour le moment.</p>
                                            <button className="px-10 py-6 bg-slate-100 text-slate-600 font-black rounded-2xl" onClick={onReset}>Affiner mon profil</button>
                                        </div>
                                    )}
                                </div>
                            </div>
                        </div>
                    )}
                </div>
            )}

            {/* DRIVING LICENSE SPECIFIC VIEW */}
            {isDrivingExchange && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    {getDrivingResult() === 'SUCCESS' ? (
                        <div className="p-12 bg-gradient-to-br from-emerald-600 to-teal-700 rounded-[3rem] text-white shadow-2xl shadow-emerald-200 relative overflow-hidden border-4 border-white">
                            <div className="absolute top-0 right-10 p-12 opacity-10">
                                <Car className="w-64 h-64" />
                            </div>
                            <div className="relative z-10 space-y-8">
                                <div className="inline-flex px-4 py-2 bg-emerald-400 text-emerald-950 rounded-full text-[10px] font-black uppercase tracking-widest">Éligible</div>
                                <h3 className="text-5xl font-black leading-tight max-w-2xl">Vous êtes dans les temps !</h3>
                                <p className="text-xl text-emerald-50 font-medium opacity-90 max-w-xl leading-relaxed">
                                    Attention, la procédure ANTS est complexe et tout rejet peut vous faire sortir du délai légal d'un an.
                                </p>

                                <div className="pt-8 flex flex-col sm:flex-row gap-6 items-center">
                                    <button
                                        onClick={() => handleCheckout(149, "Accompagnement Échange Permis")}
                                        className="group px-10 py-6 bg-white text-emerald-600 text-xl font-black rounded-2xl hover:bg-slate-50 transition-all flex items-center gap-3 shadow-xl"
                                    >
                                        Sécuriser mon échange maintenant
                                        <ArrowRight className="w-6 h-6 group-hover:translate-x-1 transition-transform" />
                                    </button>
                                    <div className="text-emerald-200 font-bold text-sm text-center sm:text-left">
                                        Service Accompagnement Permis :<br />Dossier + Photo signature + Suivi ANTS
                                    </div>
                                </div>
                            </div>
                        </div>
                    ) : (
                        <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-xl relative overflow-hidden">
                            <div className="flex flex-col md:flex-row gap-12 items-center">
                                <div className={cn(
                                    "w-32 h-32 rounded-[2.5rem] flex items-center justify-center shrink-0",
                                    getDrivingResult() === 'STUDENT_NO_NEED' ? "bg-blue-50 text-blue-500" : "bg-rose-50 text-rose-500"
                                )}>
                                    {getDrivingResult() === 'STUDENT_NO_NEED' ? <Info className="w-16 h-16" /> : <XCircle className="w-16 h-16" />}
                                </div>
                                <div className="flex-1 space-y-6">
                                    {getDrivingResult() === 'STUDENT_NO_NEED' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Pas d'échange nécessaire</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">En tant qu'étudiant, votre permis étranger est reconnu pendant toute la durée de vos études en France.</p>
                                            <button className="px-10 py-6 bg-blue-600 text-white font-black rounded-2xl hover:bg-blue-700 transition-all" onClick={onReset}>
                                                Faire un autre diagnostic
                                            </button>
                                        </div>
                                    )}

                                    {getDrivingResult() === 'TOURIST_IMPOSSIBLE' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Échange Impossible</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">L'échange de permis est réservé aux personnes résidant en France avec un titre de séjour valide.</p>
                                            <button className="px-10 py-6 bg-slate-900 text-white font-black rounded-2xl" onClick={onReset}>Retour</button>
                                        </div>
                                    )}

                                    {getDrivingResult() === 'NO_ACCORD_IMPOSSIBLE' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Échange Impossible</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">Votre pays ne dispose pas d'accord d'échange avec la France. Il est nécessaire de repasser le permis français.</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-indigo-600 text-white font-black rounded-2xl hover:bg-indigo-700 transition-all">
                                                Trouver une auto-école partenaire
                                            </button>
                                        </div>
                                    )}

                                    {getDrivingResult() === 'DELAY_EXCEEDED' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Délai dépassé</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">Le délai légal d'un an est dépassé. Votre permis étranger n'est plus reconnu sur le territoire français.</p>
                                            <div className="flex flex-wrap gap-4">
                                                <button className="px-10 py-6 bg-rose-600 text-white font-black rounded-2xl hover:bg-rose-700 transition-all shadow-lg">
                                                    S'inscrire en auto-école
                                                </button>
                                                <button className="px-10 py-6 bg-slate-100 text-slate-600 font-bold rounded-2xl hover:bg-slate-200 transition-all">
                                                    Conseil Juridique
                                                </button>
                                            </div>
                                        </div>
                                    )}

                                    {getDrivingResult() === 'UNKNOWN' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Dossier Incomplet</h3>
                                            <p className="text-xl text-slate-500 font-medium">Certaines informations sont manquantes pour évaluer votre éligibilité.</p>
                                            <button className="px-10 py-6 bg-slate-100 text-slate-600 font-black rounded-2xl" onClick={onReset}>Recommencer</button>
                                        </div>
                                    )}
                                </div>
                            </div>
                        </div>
                    )}
                </div>
            )}

            {/* RDV PREFECTURE SPECIFIC VIEW */}
            {isRdvPrefecture && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-2xl shadow-slate-200/50 relative overflow-hidden">
                        <div className="absolute top-0 right-10 p-12 opacity-5 text-indigo-600">
                            <Scale className="w-64 h-64" />
                        </div>

                        <div className="relative z-10 space-y-10">
                            <div className="space-y-4">
                                <div className="inline-flex px-4 py-2 bg-indigo-600 text-white rounded-full text-[10px] font-black uppercase tracking-widest">Offre de Service</div>
                                <h3 className="text-4xl md:text-5xl font-black text-slate-900 leading-tight">
                                    Assistance : Prise de Rendez-vous Préfecture
                                </h3>
                                <div className="flex items-center gap-2 text-indigo-600 bg-indigo-50 px-4 py-2 rounded-xl border border-indigo-100 w-fit">
                                    <MapPin className="w-5 h-5" />
                                    <span className="font-bold">Préfecture de {userProfile.rdv_prefecture.prefecture_dept?.slice(0, 2)}</span>
                                </div>
                            </div>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
                                <div className="space-y-6">
                                    <div className="space-y-3">
                                        <p className="text-sm font-black text-slate-400 uppercase tracking-widest">Votre Mission</p>
                                        <p className="text-xl text-slate-600 font-medium leading-relaxed">
                                            Obtention d'un créneau confirmé pour <span className="text-slate-900 font-bold">"{
                                                {
                                                    'retrait_titre': 'Retrait de titre',
                                                    'premiere_demande_papier': 'Première demande',
                                                    'commission_medicale': 'Commission médicale',
                                                    'renouvellement_hors_ligne': 'Renouvellement hors ligne',
                                                    'naturalisation': 'Naturalisation',
                                                    'echange_permis': 'Échange Permis',
                                                    'renouvellement_anef': 'Renouvellement ANEF'
                                                }[userProfile.rdv_prefecture.rdv_reason!] || 'votre demande'
                                            }"</span> à la préfecture compétente.
                                        </p>
                                    </div>
                                    <div className="space-y-2">
                                        <div className="flex items-center gap-3 text-emerald-600 font-bold">
                                            <CheckCircle2 className="w-5 h-5" />
                                            <span>Surveillance logicielle H24</span>
                                        </div>
                                        <div className="flex items-center gap-3 text-emerald-600 font-bold">
                                            <CheckCircle2 className="w-5 h-5" />
                                            <span>Validation dossier incluse</span>
                                        </div>
                                        <div className="flex items-center gap-3 text-emerald-600 font-bold">
                                            <CheckCircle2 className="w-5 h-5" />
                                            <span>Délai moyen : 7 à 15 jours</span>
                                        </div>
                                    </div>
                                    <div className="pt-4">
                                        <p className="text-4xl font-black text-slate-900">89,00 € <span className="text-lg text-slate-400 font-bold italic lowercase">TTC</span></p>
                                        <p className="text-sm text-slate-400 font-medium italic mt-1 font-serif">Tarif forfaitaire incluant l'accès à l'API de monitoring.</p>
                                    </div>
                                </div>

                                <div className="bg-slate-50 rounded-[2rem] p-8 border border-slate-100 space-y-6">
                                    <p className="text-sm font-black text-slate-400 uppercase tracking-widest">Étapes de votre commande</p>
                                    <div className="space-y-6">
                                        {[
                                            { step: 1, label: "Validation de la commande" },
                                            { step: 2, label: "Signature électronique du mandat" },
                                            { step: 3, label: "Paiement sécurisé (Stripe)" },
                                            { step: 4, label: "Réception de votre convocation" }
                                        ].map((item) => (
                                            <div key={item.step} className="flex items-center gap-4">
                                                <div className="w-8 h-8 bg-white border-2 border-slate-200 rounded-full flex items-center justify-center text-slate-400 font-black text-sm shrink-0">
                                                    {item.step}
                                                </div>
                                                <span className="text-slate-600 font-bold leading-tight">{item.label}</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            </div>

                            <div className="pt-6">
                                <button
                                    onClick={() => handleCheckout(89, "Rendez-vous Préfecture")}
                                    className="w-full py-8 bg-indigo-600 text-white text-2xl font-black rounded-[2rem] hover:bg-black transition-all shadow-2xl shadow-indigo-200 flex items-center justify-center gap-4 group"
                                >
                                    Commander cette prestation
                                    <ArrowRight className="w-8 h-8 group-hover:translate-x-2 transition-transform" />
                                </button>
                                <p className="text-center text-slate-400 font-medium text-sm mt-6 flex items-center justify-center gap-2">
                                    <Scale className="w-4 h-4" />
                                    Mandat de représentation exclusif Simulegal® conforme CNIL.
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* STANDARD MODE - NATURALIZATION */}
            {!isFamilyReunification && !isLegalConsultation && hasNatEligible && (
                <div className={`mb-12 p-12 bg-gradient-to-br from-amber-400 via-amber-500 to-amber-600 rounded-[3rem] text-white shadow-2xl shadow-amber-200 border-4 border-white animate-in zoom-in-95 duration-700 relative overflow-hidden ${!showNaturalizationFirst ? 'order-2 opacity-90' : 'order-1'}`}>
                    <div className="absolute top-0 right-0 -mr-8 -mt-8 w-64 h-64 bg-white/20 rounded-full blur-3xl" />
                    <div className="relative z-10 space-y-8">
                        <div className="flex items-center gap-3">
                            <span className="bg-white/20 backdrop-blur-sm text-white text-[10px] font-black px-4 py-2 rounded-full uppercase tracking-[0.2em] border border-white/30">
                                LE GRAAL
                            </span>
                            <span className="text-[10px] font-bold px-4 py-2 rounded-full border border-white/30 backdrop-blur-sm">
                                {handleProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleNaturalization[0].id).label}
                            </span>
                        </div>

                        <div className="flex items-center gap-8">
                            <div className="w-24 h-24 bg-white/30 backdrop-blur-md rounded-3xl flex items-center justify-center text-5xl shadow-inner border border-white/40">
                                🇫🇷
                            </div>
                            <div className="flex-1 space-y-2">
                                <h3 className="text-4xl font-black tracking-tight">{eligibleNaturalization[0].name}</h3>
                                <p className="text-xl text-amber-50 font-medium opacity-90 leading-tight">
                                    Excellent profil ! Vous remplissez les critères pour devenir Français.
                                </p>
                            </div>
                        </div>

                        <button
                            onClick={() => handleCheckout(990, "Accompagnement Nationalité")}
                            className="w-full sm:w-auto px-12 py-6 bg-white text-amber-600 text-xl font-black rounded-2xl hover:bg-amber-50 transition-all shadow-xl uppercase tracking-wider"
                        >
                            Lancer ma demande maintenant
                        </button>
                    </div>
                </div>
            )}

            {/* STANDARD MODE - RESIDENCY */}
            {!isFamilyReunification && (
                <div className={`space-y-12 flex flex-col ${showNaturalizationFirst ? 'order-2' : 'order-1'}`}>
                    {hasStayEligible ? (
                        <section className="animate-in slide-in-from-bottom-8 duration-700">
                            <div className="flex items-center gap-6 mb-8">
                                <h3 className="text-xs font-black text-slate-400 uppercase tracking-[0.4em] whitespace-nowrap">
                                    {targetGoal === 'NATURALIZATION' ? 'Vos alternatives séjour' : 'Votre meilleure option'}
                                </h3>
                                <div className="h-0.5 bg-slate-100 flex-1" />
                            </div>

                            <div className="relative group">
                                <div className="p-10 rounded-[3rem] relative overflow-hidden border-4 border-indigo-50 bg-white shadow-2xl shadow-indigo-100/40 transition-all duration-300">
                                    <div className="flex flex-col lg:flex-row gap-12 items-start relative z-10">
                                        <div className="flex-1 space-y-8">
                                            <div className="flex flex-wrap items-center gap-4">
                                                <span className={`px-4 py-2 rounded-xl text-xs font-black uppercase tracking-widest shadow-sm ${handleProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleStays[0].id).color}`}>
                                                    {handleProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleStays[0].id).label}
                                                </span>
                                            </div>

                                            <div className="space-y-4">
                                                <h4 className="text-4xl font-black text-slate-900 tracking-tight">{eligibleStays[0].name}</h4>
                                                <p className="text-xl text-slate-500 leading-relaxed font-medium max-w-3xl">
                                                    {eligibleStays[0].description}
                                                </p>
                                            </div>

                                            <div className="flex flex-wrap gap-6 pt-4">
                                                <button className="px-10 py-5 bg-slate-900 text-white font-black rounded-2xl hover:bg-slate-800 transition-all shadow-xl">
                                                    Détails de la procédure
                                                </button>
                                                <button className="px-10 py-5 bg-white text-slate-600 font-bold rounded-2xl border-2 border-slate-100 hover:border-slate-200 transition-all">
                                                    Liste des pièces
                                                </button>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </section>
                    ) : (
                        (!hasNatEligible || targetGoal !== 'NATURALIZATION') && (
                            <div className="p-20 text-center bg-slate-50 rounded-[4rem] border-4 border-dashed border-slate-100">
                                <div className="text-6xl mb-8">🔍</div>
                                <h3 className="text-3xl font-black text-slate-900 mb-4">Aucune éligibilité immédiate</h3>
                                <p className="text-xl text-slate-500 max-w-xl mx-auto font-medium">
                                    Nos algorithmes n'ont pas trouvé de titre de séjour correspondant exactement à votre profil actuel. Essayez d'ajuster vos critères.
                                </p>
                            </div>
                        )
                    )}
                </div>
            )}

            <div className="mt-auto pt-12 border-t border-slate-100 opacity-50">
                <p className="text-sm text-slate-400 font-bold text-center uppercase tracking-widest">
                    Simulégale © 2026 • Confidentialité garantie • Analyse CESEDA v2.4
                </p>
            </div>
            {/* RDV JURISTE SPECIFIC VIEW */}
            {isLegalConsultation && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-2xl shadow-slate-200/50 relative overflow-hidden">
                        <div className="absolute top-0 right-10 p-12 opacity-5 text-indigo-600">
                            <Scale className="w-64 h-64" />
                        </div>

                        <div className="relative z-10 space-y-10">
                            <div className="space-y-4">
                                <div className="inline-flex px-4 py-2 bg-indigo-600 text-white rounded-full text-[10px] font-black uppercase tracking-widest">Offre de Consultation</div>
                                <h3 className="text-4xl md:text-5xl font-black text-slate-900 leading-tight">
                                    {userProfile.rdv_juriste.mode === 'remote' ? 'Consultation Juridique Express (Visio)' : 'Consultation Premium en Cabinet'}
                                </h3>
                                <div className="flex items-center gap-2 text-indigo-600 bg-indigo-50 px-4 py-2 rounded-xl border border-indigo-100 w-fit">
                                    <FileText className="w-5 h-5" />
                                    <span className="font-bold">Sujet : {
                                        {
                                            'oqtf_contentieux': 'OQTF / Contentieux (Urgence)',
                                            'refus_recours': 'Refus de titre / Recours',
                                            'conseil_dossier': 'Conseil sur dossier',
                                            'verification_dossier': 'Vérification de dossier',
                                            'autre': 'Autre question juridique'
                                        }[userProfile.rdv_juriste.subject!] || 'Votre situation'
                                    }</span>
                                </div>
                            </div>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
                                <div className="space-y-6">
                                    <div className="space-y-3">
                                        <p className="text-sm font-black text-slate-400 uppercase tracking-widest">Contenu de la séance</p>
                                        <div className="space-y-4">
                                            {userProfile.rdv_juriste.mode === 'remote' ? (
                                                <>
                                                    <div className="flex items-center gap-3 text-slate-600 font-bold">
                                                        <CheckCircle2 className="w-5 h-5 text-emerald-500" />
                                                        <span>Analyse de votre situation actuelle</span>
                                                    </div>
                                                    <div className="flex items-center gap-3 text-slate-600 font-bold">
                                                        <CheckCircle2 className="w-5 h-5 text-emerald-500" />
                                                        <span>Réponses directes à vos questions</span>
                                                    </div>
                                                    <div className="flex items-center gap-3 text-slate-600 font-bold">
                                                        <CheckCircle2 className="w-5 h-5 text-emerald-500" />
                                                        <span>Stratégie écrite envoyée par mail</span>
                                                    </div>
                                                </>
                                            ) : (
                                                <>
                                                    <div className="flex items-center gap-3 text-slate-600 font-bold">
                                                        <CheckCircle2 className="w-5 h-5 text-emerald-500" />
                                                        <span>Étude complète des pièces originales</span>
                                                    </div>
                                                    <div className="flex items-center gap-3 text-slate-600 font-bold">
                                                        <CheckCircle2 className="w-5 h-5 text-emerald-500" />
                                                        <span>Définition de la stratégie procédurale</span>
                                                    </div>
                                                    <div className="flex items-center gap-3 text-slate-600 font-bold">
                                                        <CheckCircle2 className="w-5 h-5 text-emerald-500" />
                                                        <span>Rédaction de courrier type (si nécessaire)</span>
                                                    </div>
                                                </>
                                            )}
                                        </div>
                                    </div>
                                    <div className="pt-4">
                                        <p className="text-4xl font-black text-slate-900">
                                            {userProfile.rdv_juriste.mode === 'remote' ? '69,00 €' : '99,00 €'}
                                            <span className="text-lg text-slate-400 font-bold italic lowercase ml-2 text-indigo-400">
                                                / {userProfile.rdv_juriste.mode === 'remote' ? '30 min' : '45 min'}
                                            </span>
                                        </p>
                                        <p className="text-sm text-slate-400 font-medium italic mt-1 font-serif">Paiement sécurisé via Stripe.</p>
                                    </div>
                                </div>

                                <div className="bg-slate-50 rounded-[2rem] p-8 border border-slate-100 space-y-6">
                                    <p className="text-sm font-black text-slate-400 uppercase tracking-widest">Déroulement</p>
                                    <div className="space-y-6">
                                        {[
                                            { step: 1, label: "Réservation du créneau" },
                                            { step: 2, label: "Paiement de la consultation" },
                                            { step: 3, label: userProfile.rdv_juriste.mode === 'remote' ? "Réception du lien Visio" : "Confirmation de l'adresse" },
                                            { step: 4, label: "Échange avec votre expert" }
                                        ].map((item) => (
                                            <div key={item.step} className="flex items-center gap-4">
                                                <div className="w-8 h-8 bg-white border-2 border-slate-200 rounded-full flex items-center justify-center text-slate-400 font-black text-sm shrink-0">
                                                    {item.step}
                                                </div>
                                                <span className="text-slate-600 font-bold leading-tight">{item.label}</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            </div>

                            <div className="pt-6">
                                <button
                                    onClick={() => handleCheckout(userProfile.rdv_juriste.mode === 'remote' ? 69 : 99, `Consultation Juridique ${userProfile.rdv_juriste.mode}`)}
                                    className="w-full py-8 bg-indigo-600 text-white text-2xl font-black rounded-[2rem] hover:bg-black transition-all shadow-2xl shadow-indigo-200 flex items-center justify-center gap-4 group"
                                >
                                    {userProfile.rdv_juriste.mode === 'remote' ? 'Réserver mon créneau Visio' : 'Réserver mon créneau Agence'}
                                    <ArrowRight className="w-8 h-8 group-hover:translate-x-2 transition-transform" />
                                </button>
                                <p className="text-center text-slate-400 font-medium text-sm mt-6 flex items-center justify-center gap-2">
                                    <Smartphone className="w-4 h-4" />
                                    Confirmation instantanée par SMS et Email.
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* CALLBACK SPECIFIC VIEW (LEAD GEN) */}
            {isCallback && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-2xl shadow-slate-200/50 relative overflow-hidden">
                        <div className="absolute top-0 right-10 p-12 opacity-5 text-indigo-600">
                            <Phone className="w-64 h-64" />
                        </div>

                        <div className="relative z-10 space-y-10">
                            <div className="space-y-4">
                                <div className={cn(
                                    "inline-flex px-4 py-2 rounded-full text-[10px] font-black uppercase tracking-widest border",
                                    userProfile.callback.callback_subject === 'URGENT_LEGAL'
                                        ? "bg-rose-100 text-rose-700 border-rose-200"
                                        : "bg-indigo-100 text-indigo-700 border-indigo-200"
                                )}>
                                    {userProfile.callback.callback_subject === 'URGENT_LEGAL' ? '🚨 Priorité Urgence Activée' : 'Demande de rappel pré-enregistrée'}
                                </div>
                                <h3 className="text-4xl md:text-5xl font-black text-slate-900 leading-tight">
                                    {userProfile.callback.callback_subject === 'URGENT_LEGAL' ? 'Cellule d\'intervention juridique' : 'Analyse de votre demande'}
                                </h3>
                                <p className="text-xl text-slate-500 font-medium leading-relaxed">
                                    Nous avons identifié l'agence compétente dans le secteur <span className="text-slate-900 font-bold">{userProfile.callback.location_zip}</span>. Un expert va prendre connaissance de votre demande.
                                </p>
                            </div>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
                                <div className="space-y-6">
                                    <div className="space-y-4">
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-indigo-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-indigo-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Votre demande sera traitée par un expert local</span>
                                        </div>
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-indigo-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-indigo-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Confidentialité totale des échanges</span>
                                        </div>
                                        {userProfile.callback.callback_subject === 'URGENT_LEGAL' && (
                                            <div className="flex items-center gap-4 p-4 bg-rose-50 rounded-2xl border border-rose-100 text-rose-700">
                                                <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-rose-600 shadow-sm">
                                                    <AlertCircle size={24} />
                                                </div>
                                                <span className="font-bold">Délai de rappel moyen : &lt; 15 minutes</span>
                                            </div>
                                        )}
                                    </div>
                                </div>

                                <div className="bg-slate-50 rounded-[2rem] p-10 border border-slate-100 flex flex-col justify-center text-center space-y-4">
                                    <div className="text-5xl mb-2">📞</div>
                                    <h4 className="text-2xl font-black text-slate-900">Mise en relation</h4>
                                    <p className="text-slate-500 font-medium">
                                        Agent disponible.
                                    </p>
                                </div>
                            </div>

                            <div className="pt-6">
                                <button
                                    onClick={() => setShowLeadModal(true)}
                                    className="w-full py-8 bg-slate-900 text-white text-2xl font-black rounded-[2rem] hover:bg-black transition-all shadow-2xl flex items-center justify-center gap-4 group"
                                >
                                    Valider mon numéro de téléphone
                                    <ArrowRight className="w-8 h-8 group-hover:translate-x-2 transition-transform" />
                                </button>
                                <p className="text-center text-slate-400 font-medium text-sm mt-6 flex items-center justify-center gap-2">
                                    <Clock className="w-4 h-4" />
                                    Horaires d'ouverture : 9h00 - 19h00
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* CIVIC EXAM SPECIFIC VIEW (LEAD GEN) */}
            {isCivicExam && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-2xl shadow-indigo-100/50 relative overflow-hidden">
                        <div className="absolute top-0 right-10 p-12 opacity-5 text-indigo-600">
                            <GraduationCap className="w-64 h-64" />
                        </div>

                        <div className="relative z-10 space-y-10">
                            <div className="space-y-4">
                                <div className="inline-flex px-4 py-2 bg-indigo-100 text-indigo-700 rounded-full text-[10px] font-black uppercase tracking-widest border border-indigo-200">
                                    Profil Validé
                                </div>
                                <h3 className="text-4xl md:text-5xl font-black text-slate-900 leading-tight">
                                    Préparez votre examen civique dans le {userProfile.civic_exam.location_zip}
                                </h3>
                                <p className="text-xl text-slate-500 font-medium">
                                    Ne prenez pas le risque d'un refus. Nos formateurs partenaires vous préparent au QCM et à l'entretien pour votre objectif {
                                        userProfile.civic_exam.civic_goal === 'NATURALIZATION' ? 'Naturalisation' :
                                            userProfile.civic_exam.civic_goal === 'RESIDENCE' ? 'Carte de Résident' : 'Seconde Chance'
                                    }.
                                </p>
                            </div>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
                                <div className="space-y-6">
                                    <div className="space-y-4">
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-indigo-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-indigo-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Quiz d'entraînement aux 50 questions types</span>
                                        </div>
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-indigo-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-indigo-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Simulation d'entretien d'assimilation</span>
                                        </div>
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-indigo-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-indigo-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Ateliers 'Valeurs de la République' agréés</span>
                                        </div>
                                    </div>
                                </div>

                                <div className="bg-emerald-50/50 rounded-[2rem] p-10 border border-emerald-100 flex flex-col justify-center text-center space-y-4">
                                    <div className="text-5xl mb-2">🎓</div>
                                    <h4 className="text-2xl font-black text-emerald-900">Formateurs disponibles</h4>
                                    <p className="text-emerald-600 font-medium">
                                        Sessions individuelles ou collectives.
                                    </p>
                                </div>
                            </div>

                            <div className="pt-6">
                                <button
                                    onClick={() => setShowLeadModal(true)}
                                    className="w-full py-8 bg-slate-900 text-white text-2xl font-black rounded-[2rem] hover:bg-black transition-all shadow-2xl flex items-center justify-center gap-4 group"
                                >
                                    Être contacté par un formateur
                                    <ArrowRight className="w-8 h-8 group-hover:translate-x-2 transition-transform" />
                                </button>
                                <p className="text-center text-slate-400 font-medium text-sm mt-6">
                                    Réponse sous 2 heures (jours ouvrés).
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* FRENCH COURSE SPECIFIC VIEW (LEAD GEN) */}
            {isFrenchCourse && (
                <div className="space-y-8 animate-in fade-in zoom-in-95 duration-500 flex-1 flex flex-col justify-center">
                    <div className="p-12 bg-white border-4 border-slate-100 rounded-[3rem] shadow-2xl shadow-indigo-100/50 relative overflow-hidden">
                        <div className="absolute top-0 right-10 p-12 opacity-5 text-indigo-600">
                            <Languages className="w-64 h-64" />
                        </div>

                        <div className="relative z-10 space-y-10">
                            <div className="space-y-4">
                                <div className="inline-flex px-4 py-2 bg-emerald-100 text-emerald-700 rounded-full text-[10px] font-black uppercase tracking-widest border border-emerald-200">
                                    Profil Validé
                                </div>
                                <h3 className="text-4xl md:text-5xl font-black text-slate-900 leading-tight">
                                    Centres disponibles dans le {userProfile.french.location_zip}
                                </h3>
                                <p className="text-xl text-slate-500 font-medium">
                                    Nous avons sélectionné 2 agences partenaires pour votre objectif {
                                        userProfile.french.goal === 'NATURALIZATION' ? 'Naturalisation' :
                                            userProfile.french.goal === 'RESIDENCE' ? 'Carte de Résident' : 'Études / Pro'
                                    }.
                                </p>
                            </div>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
                                <div className="space-y-6">
                                    <div className="space-y-4">
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-emerald-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-emerald-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Centres agréés Préfecture / OFII</span>
                                        </div>
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-emerald-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-emerald-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Test de positionnement gratuit</span>
                                        </div>
                                        <div className="flex items-center gap-4 p-4 bg-slate-50 rounded-2xl border border-slate-100 group hover:border-emerald-200 transition-colors">
                                            <div className="w-10 h-10 bg-white rounded-xl flex items-center justify-center text-emerald-600 shadow-sm">
                                                <CheckCircle2 size={24} />
                                            </div>
                                            <span className="font-bold text-slate-700">Éligible CPF / Financement OPCO</span>
                                        </div>
                                    </div>
                                </div>

                                <div className="bg-indigo-50/50 rounded-[2rem] p-10 border border-indigo-100 flex flex-col justify-center text-center space-y-4">
                                    <div className="text-5xl mb-2">🎯</div>
                                    <h4 className="text-2xl font-black text-indigo-900">3 centres trouvés</h4>
                                    <p className="text-indigo-600 font-medium">
                                        À moins de 15 min de chez vous.
                                    </p>
                                </div>
                            </div>

                            <div className="pt-6">
                                <button
                                    onClick={() => setShowLeadModal(true)}
                                    className="w-full py-8 bg-emerald-600 text-white text-2xl font-black rounded-[2rem] hover:bg-emerald-700 transition-all shadow-2xl shadow-emerald-200 flex items-center justify-center gap-4 group"
                                >
                                    Être rappelé par un conseiller local
                                    <ArrowRight className="w-8 h-8 group-hover:translate-x-2 transition-transform" />
                                </button>
                                <p className="text-center text-slate-400 font-medium text-sm mt-6">
                                    Réponse sous 2 heures (jours ouvrés).
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* LEAD GENERATION MODAL */}
            {showLeadModal && (
                <div className="fixed inset-0 z-[100] flex items-center justify-center p-6 backdrop-blur-xl bg-slate-900/40 animate-in fade-in duration-300">
                    <div className="bg-white w-full max-w-xl rounded-[3.5rem] shadow-2xl relative overflow-hidden animate-in zoom-in-95 slide-in-from-bottom-10 duration-500">
                        <button
                            onClick={() => setShowLeadModal(false)}
                            className="absolute top-10 right-10 p-3 hover:bg-slate-100 rounded-full transition-colors text-slate-400 hover:text-slate-900"
                        >
                            <XCircle className="w-6 h-6" />
                        </button>

                        <div className="p-16 space-y-10">
                            <div className="space-y-4">
                                <div className="w-20 h-20 bg-emerald-50 text-emerald-600 rounded-[2rem] flex items-center justify-center mb-6">
                                    <Smartphone className="w-10 h-10" />
                                </div>
                                <h2 className="text-4xl font-black text-slate-900 leading-tight">Demande de rappel</h2>
                                <p className="text-xl text-slate-500 font-medium">
                                    Un conseiller spécialisé vous contactera pour organiser votre test de niveau gratuit.
                                </p>
                            </div>

                            <div className="space-y-6">
                                <div className="space-y-2">
                                    <label className="text-sm font-black text-slate-400 uppercase tracking-widest ml-2">Nom Complet</label>
                                    <input
                                        type="text"
                                        value={leadForm.name}
                                        onChange={(e) => setLeadForm({ ...leadForm, name: e.target.value })}
                                        placeholder="Jean Dupont"
                                        className="w-full bg-slate-50 border-2 border-slate-100 rounded-2xl px-8 py-5 text-xl font-bold focus:outline-none focus:border-emerald-600 focus:bg-white transition-all"
                                    />
                                </div>
                                <div className="space-y-2">
                                    <label className="text-sm font-black text-slate-400 uppercase tracking-widest ml-2">Numéro de Téléphone</label>
                                    <input
                                        type="tel"
                                        value={leadForm.phone}
                                        onChange={(e) => setLeadForm({ ...leadForm, phone: e.target.value })}
                                        placeholder="06 12 34 56 78"
                                        className="w-full bg-slate-50 border-2 border-slate-100 rounded-2xl px-8 py-5 text-xl font-bold focus:outline-none focus:border-emerald-600 focus:bg-white transition-all"
                                    />
                                </div>

                                <button
                                    onClick={() => {
                                        alert(`Merci ${leadForm.name} ! Un conseiller va vous rappeler au ${leadForm.phone}.`);
                                        setShowLeadModal(false);
                                    }}
                                    disabled={!leadForm.name || !leadForm.phone}
                                    className="w-full py-8 bg-slate-900 text-white text-2xl font-black rounded-[2rem] hover:bg-black transition-all shadow-2xl disabled:opacity-50 disabled:cursor-not-allowed mt-4"
                                >
                                    Valider ma demande
                                </button>
                            </div>
                        </div>

                        <div className="bg-slate-50 py-8 text-center text-[10px] font-black text-slate-400 uppercase tracking-[0.4em]">
                            Assistance Partenaire SimuLegal
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
