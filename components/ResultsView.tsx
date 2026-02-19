'use client';

import React, { useMemo } from 'react';
import { UserProfile, ProcedureRule } from '@/types';
import { evaluateRule } from '@/lib/engine';
import EligibilityStore from '@/services/EligibilityStore';
import { ArrowRight, Bell, Scale, CheckCircle2, AlertTriangle, AlertCircle, FileText, Smartphone, Car, Info, XCircle, MapPin, Languages, GraduationCap, Phone, Clock, Home } from 'lucide-react';

import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

function cn(...inputs: ClassValue[]) {
    return twMerge(clsx(inputs));
}

interface ResultsViewProps {
    userProfile: UserProfile;
    onReset: () => void;
    serviceId?: string;
    forceAgencyId?: string;
}

export default function ResultsView({ userProfile, onReset, serviceId, forceAgencyId }: ResultsViewProps) {
    const [showLeadModal, setShowLeadModal] = React.useState(false);
    const [leadForm, setLeadForm] = React.useState({ name: '', phone: '' });
    const isFamilyReunification = serviceId === 'regroupement_familial';
    const isDrivingExchange = serviceId === 'permis_conduire';
    const isRdvPrefecture = serviceId === 'rdv_prefecture';
    const isLegalConsultation = serviceId === 'rdv_juriste';
    const isFrenchCourse = serviceId === 'french_course';
    const isCivicExam = serviceId === 'examen_civique';
    const isCallback = serviceId === 'rappel_echeances';


    // Pre-calculate derived fields for rules
    const augmentedProfile = useMemo(() => {
        const p = JSON.parse(JSON.stringify(userProfile)); // Deep clone to avoid mutation issues

        // Driving: residence_duration_months
        if (p.driving?.residence_start_date) {
            try {
                const [mStr, yStr] = p.driving.residence_start_date.split('/');
                const month = parseInt(mStr, 10);
                const year = parseInt(yStr, 10);
                if (!isNaN(month) && !isNaN(year)) {
                    const startDate = new Date(year, month - 1, 1);
                    const today = new Date();
                    const diff = (today.getFullYear() - startDate.getFullYear()) * 12 + (today.getMonth() - startDate.getMonth());
                    p.driving.residence_duration_months = Math.max(0, diff);
                }
            } catch (e) {
                console.warn('Invalid date format', p.driving.residence_start_date);
            }
        }
        return p;
    }, [userProfile]);

    const drivingResults = useMemo(() => {
        if (!isDrivingExchange) return [];
        const rulesPermis = EligibilityStore.getRules('permis');
        return rulesPermis.filter((rule) => evaluateRule(augmentedProfile, rule.conditions));
    }, [augmentedProfile, isDrivingExchange]);

    const getDrivingResult = () => {
        const { status, license_country, residence_start_date } = userProfile.driving;

        // 1. Success cases via Rule Engine
        if (drivingResults.some(r => r.id === 'echange_permis_standard' || r.id === 'echange_permis_refugie')) return 'SUCCESS';
        if (drivingResults.some(r => r.id === 'echange_permis_etudiant')) return 'STUDENT_NO_NEED';

        // 2. Failure Analysis (fallback logic remains useful for specific error messages)
        if (status === 'TOURIST') return 'TOURIST_IMPOSSIBLE';
        if (license_country === 'NO_ACCORD') return 'NO_ACCORD_IMPOSSIBLE';

        if (!residence_start_date && status !== 'STUDENT') return 'UNKNOWN';

        // Check delay specifically if relevant
        if (augmentedProfile.driving?.residence_duration_months > 12) return 'DELAY_EXCEEDED';

        return 'UNKNOWN'; // Should ideally be covered by one of the above
    };

    const familyResults = useMemo(() => {
        if (!isFamilyReunification) return [];
        const rulesFamily = EligibilityStore.getRules('family');
        return rulesFamily
            .filter((rule) => evaluateRule(userProfile, rule.conditions));
    }, [userProfile, isFamilyReunification]);

    /* ─── Tier-based ranking helpers ─── */
    const TIER_ORDER: Record<string, number> = { PREMIUM: 3, STANDARD: 2, FALLBACK: 1 };
    const tierSort = (a: ProcedureRule, b: ProcedureRule) => {
        const ta = TIER_ORDER[a.tier || 'FALLBACK'] || 0;
        const tb = TIER_ORDER[b.tier || 'FALLBACK'] || 0;
        if (ta !== tb) return tb - ta;                         // tier DESC
        if ((a.duration_years || 0) !== (b.duration_years || 0))
            return (b.duration_years || 0) - (a.duration_years || 0); // duration DESC
        return b.priority - a.priority;                        // priority as tiebreaker
    };

    const tierBadge = (rule: ProcedureRule) => {
        const t = rule.tier || 'FALLBACK';
        if (t === 'PREMIUM') return { label: '⭐ Meilleure option', cls: 'bg-amber-100 text-amber-800 border-amber-200' };
        if (t === 'STANDARD') return { label: '✅ Bonne option', cls: 'bg-emerald-50 text-emerald-700 border-emerald-200' };
        return { label: '📋 Option de repli', cls: 'bg-slate-100 text-slate-500 border-slate-200' };
    };

    const durationLabel = (rule: ProcedureRule) => {
        const y = rule.duration_years;
        if (!y || y === 0) return 'Acquisition de nationalité';
        return `Durée : ${y} an${y > 1 ? 's' : ''}`;
    };

    const eligibleStays = useMemo(() => {
        if (isFamilyReunification) return [];
        const rulesSejour = EligibilityStore.getRules('sejour');
        let results = rulesSejour
            .filter((rule) => evaluateRule(userProfile, rule.conditions));

        if (userProfile.admin.current_visa_type === 'RESIDENT_CARD') {
            const allowedResidencyIds = ['carte_resident_longue_duree_ue', 'carte_resident_refugie', 'carte_resident_apatride_apres_4ans', 'carte_resident_conjoint_francais', 'carte_resident_regroupement_familial'];
            results = results.filter(r => allowedResidencyIds.includes(r.id));
        }

        // Tier-based sort: PREMIUM > STANDARD > FALLBACK, then duration DESC
        results.sort(tierSort);

        // Filter FALLBACK if better tiers exist
        const hasBetterTier = results.some(r => r.tier === 'PREMIUM' || r.tier === 'STANDARD');
        if (hasBetterTier) {
            results = results.filter(r => r.tier !== 'FALLBACK');
        }

        // Separate: keep only FIRST_OR_RENEWAL for the main section
        return results.filter(r => r.request_type !== 'UPGRADE');
    }, [userProfile, isFamilyReunification]);

    /* ─── Upgrade path results (parcours ascendant) ─── */
    const eligibleUpgrades = useMemo(() => {
        if (isFamilyReunification) return [];
        const rulesSejour = EligibilityStore.getRules('sejour');
        return rulesSejour
            .filter((rule) => rule.request_type === 'UPGRADE')
            .filter((rule) => evaluateRule(userProfile, rule.conditions))
            .sort(tierSort);
    }, [userProfile, isFamilyReunification]);

    const eligibleNaturalization = useMemo(() => {
        if (isFamilyReunification) return [];
        const rulesNaturalisation = EligibilityStore.getRules('naturalisation');
        return rulesNaturalisation
            .filter((rule) => evaluateRule(userProfile, rule.conditions))
            .sort(tierSort);
    }, [userProfile, isFamilyReunification]);

    const targetGoal = userProfile.project.target_goal || 'BOTH';
    const hasNatEligible = eligibleNaturalization.length > 0;
    const hasStayEligible = eligibleStays.length > 0;
    const isFamilyEligible = isFamilyReunification && familyResults.length > 0;

    const getFamilyFailureReason = () => {
        // Semantic checks — priority-ordered, no dependency on rules_family.json

        // 1. POLYGAMY — absolute exclusion (CESEDA L434-6)
        if (userProfile.family.is_polygamous === true) return 'POLYGAMY';

        // 2. TITRE DE SÉJOUR — must have valid permit (CESEDA L434-1)
        if (userProfile.family.rf_has_valid_titre_sejour === false) return 'TITRE_SEJOUR';

        // 3. MARITAL STATUS — must be married (CESEDA L434-2)
        const marital = userProfile.family.rf_marital_status;
        if (marital && marital !== 'MARRIED' && marital !== 'SINGLE') return 'MARITAL';

        // 4. DURATION — presence in France
        const duration = userProfile.family.presence_duration;
        const isAlgerian = userProfile.family.sponsor_nationality === 'ALGERIAN';
        if (duration === 'LESS_12') return 'DURATION';
        if (duration === '12_18' && !isAlgerian) return 'DURATION';

        // 5. RESOURCES — income source and amount
        if (userProfile.family.has_handicap_allowance === false) {
            if (userProfile.family.income_source === 'RSA_ALOWANCE') return 'SOURCE_RSA';
            if ((userProfile.work.salary_monthly_gross || 0) < 1398) return 'AMOUNT_LOW';
        }

        // 6. HOUSING — must have a home
        const housing = userProfile.family.housing_status;
        if (housing === 'SEARCHING' || housing === 'UNKNOWN' || !housing) return 'HOUSING';

        // 7. SURFACE — minimum surface based on family composition
        const memberCount = userProfile.family.rf_family_members_count || 1;
        const minSurface = 16 + Math.max(0, (memberCount - 1)) * 9;
        if ((userProfile.family.rf_housing_surface || 0) < minSurface) return 'SURFACE';

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

    const [isProcessing, setIsProcessing] = React.useState(false);
    const [pendingAction, setPendingAction] = React.useState<{ amount: number, label: string } | null>(null);

    const performCheckout = async (contactInfo: { name: string, phone: string, email?: string }) => {
        setIsProcessing(true);
        try {
            const amount = pendingAction?.amount || 0;
            const label = pendingAction?.label || 'Service Simulegal';

            // 1. Create Lead
            const leadPayload = {
                name: contactInfo.name,
                email: contactInfo.email || `${contactInfo.phone.replace(/\D/g, '')}@no-email.com`, // Fallback
                phone: contactInfo.phone,
                serviceId: serviceId || 'generic',
                serviceName: label,
                originAgencyId: forceAgencyId,
                data: JSON.stringify(userProfile),
                status: 'NEW' // Explicitly set status to NEW
            };

            const leadRes = await fetch(`${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3005'}/public/leads`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(leadPayload)
            });

            if (!leadRes.ok) throw new Error('Erreur lors de la création du dossier');
            const lead = await leadRes.json();

            // 2. Create Checkout Session
            const checkoutRes = await fetch(`${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3005'}/public/leads/checkout`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    leadId: lead.id,
                    amount: amount,
                    label: label,
                    successUrl: `${window.location.origin}/success`,
                    cancelUrl: `${window.location.origin}/cancel`
                })
            });

            if (!checkoutRes.ok) throw new Error('Erreur lors de l\'initialisation du paiement');
            const session = await checkoutRes.json();

            // 3. Redirect to Payment
            window.location.href = session.url;

        } catch (error) {
            console.error('Checkout error:', error);
            alert("Une erreur est survenue lors de la préparation de votre commande. Veuillez réessayer.");
            setIsProcessing(false);
            setPendingAction(null);
        }
    };

    const handleCheckout = (amount: number, label: string) => {
        // Prepare action
        setPendingAction({ amount, label });

        // Check if we have contact info in profile
        if (userProfile.identity?.name && (userProfile.identity?.phone || userProfile.identity?.email)) {
            performCheckout({
                name: userProfile.identity.name,
                phone: userProfile.identity.phone || '',
                email: userProfile.identity.email
            });
        } else {
            // Open modal to ask for contact info
            setShowLeadModal(true);
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
                                        onClick={() => handleCheckout(390, "Accompagnement Regroupement Familial")}
                                        className="group px-10 py-6 bg-white text-indigo-600 text-xl font-black rounded-2xl hover:bg-slate-50 transition-all flex items-center gap-3 shadow-xl"
                                    >
                                        Commander — 390€
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
                                    {getFamilyFailureReason() === 'POLYGAMY' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Regroupement familial interdit</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">L'article L434-6 du CESEDA interdit le regroupement familial en situation de polygamie. Cette exclusion est absolue et ne peut faire l'objet d'aucune dérogation.</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-slate-900 text-white font-black rounded-2xl hover:bg-slate-800 transition-all shadow-lg">
                                                <Scale className="w-6 h-6" />
                                                Consulter un juriste
                                            </button>
                                        </div>
                                    )}

                                    {getFamilyFailureReason() === 'TITRE_SEJOUR' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Titre de séjour requis</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">L'article L434-1 du CESEDA exige un titre de séjour en cours de validité d'une durée d'au moins 1 an. Vous devez d'abord régulariser votre situation administrative.</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-slate-900 text-white font-black rounded-2xl hover:bg-slate-800 transition-all shadow-lg">
                                                <Scale className="w-6 h-6" />
                                                Consulter un juriste
                                            </button>
                                        </div>
                                    )}

                                    {getFamilyFailureReason() === 'MARITAL' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Situation conjugale non éligible</h3>
                                            <p className="text-xl text-slate-500 font-medium leading-relaxed">Le regroupement familial est réservé aux conjoints mariés (CESEDA L434-2). Si vous êtes pacsé(e) ou concubin(e), vous pouvez demander un titre « vie privée et familiale » — c'est une procédure différente.</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-indigo-600 text-white font-black rounded-2xl hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-100">
                                                <Scale className="w-6 h-6" />
                                                Explorer les alternatives
                                            </button>
                                        </div>
                                    )}

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

                                    {getFamilyFailureReason() === 'SURFACE' && (
                                        <div className="space-y-4">
                                            <h3 className="text-4xl font-black text-slate-900 leading-tight">Surface du logement insuffisante</h3>
                                            <p className="text-xl text-slate-500 font-medium">Pour {userProfile.family.rf_family_members_count || 1} personne(s), la surface minimale requise est de {16 + Math.max(0, ((userProfile.family.rf_family_members_count || 1) - 1)) * 9} m². Votre logement fait {userProfile.family.rf_housing_surface || 0} m².</p>
                                            <button className="flex items-center gap-4 px-10 py-6 bg-amber-600 text-white font-black rounded-2xl hover:bg-amber-700 transition-all shadow-lg shadow-amber-100">
                                                <Home className="w-6 h-6" />
                                                Consulter le Guide Surface OFII
                                            </button>
                                            <p className="text-xs text-slate-400 font-bold uppercase tracking-[0.1em]">Règle : 16 m² pour le couple + 9 m² par personne supplémentaire (CESEDA R434-15).</p>
                                        </div>
                                    )}

                                    {getFamilyFailureReason() === 'UNKNOWN' && (
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

                            {/* ─── Best option card ─── */}
                            <div className="relative group">
                                <div className="p-10 rounded-[3rem] relative overflow-hidden border-4 border-indigo-50 bg-white shadow-2xl shadow-indigo-100/40 transition-all duration-300">
                                    <div className="flex flex-col lg:flex-row gap-12 items-start relative z-10">
                                        <div className="flex-1 space-y-8">
                                            <div className="flex flex-wrap items-center gap-4">
                                                <span className={`px-4 py-2 rounded-xl text-xs font-black uppercase tracking-widest shadow-sm ${handleProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleStays[0].id).color}`}>
                                                    {handleProcedureDetails(userProfile.admin.current_visa_type || 'NONE', eligibleStays[0].id).label}
                                                </span>
                                                <span className={`px-3 py-1.5 rounded-lg text-[10px] font-bold border ${tierBadge(eligibleStays[0]).cls}`}>
                                                    {tierBadge(eligibleStays[0]).label}
                                                </span>
                                                <span className="px-3 py-1.5 rounded-lg text-[10px] font-bold bg-indigo-50 text-indigo-600 border border-indigo-100">
                                                    {durationLabel(eligibleStays[0])}
                                                </span>
                                                {eligibleStays[0].gives_work_right && (
                                                    <span className="px-3 py-1.5 rounded-lg text-[10px] font-bold bg-green-50 text-green-600 border border-green-100">
                                                        💼 Droit au travail
                                                    </span>
                                                )}
                                            </div>

                                            <div className="space-y-4">
                                                <h4 className="text-4xl font-black text-slate-900 tracking-tight">{eligibleStays[0].name}</h4>
                                                <p className="text-xl text-slate-500 leading-relaxed font-medium max-w-3xl">
                                                    {eligibleStays[0].description}
                                                </p>
                                            </div>

                                            <div className="flex flex-wrap gap-6 pt-4">
                                                <button
                                                    onClick={() => handleCheckout(290, "Accompagnement Titre de Séjour")}
                                                    className="px-10 py-5 bg-indigo-600 text-white font-black rounded-2xl hover:bg-slate-900 transition-all shadow-xl"
                                                >
                                                    Commander mon accompagnement
                                                </button>
                                                <button className="px-10 py-5 bg-white text-slate-600 font-bold rounded-2xl border-2 border-slate-100 hover:border-slate-200 transition-all">
                                                    Liste des pièces
                                                </button>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>

                            {/* ─── Alternatives section ─── */}
                            {eligibleStays.length > 1 && (
                                <div className="mt-10">
                                    <div className="flex items-center gap-6 mb-6">
                                        <h3 className="text-xs font-black text-slate-400 uppercase tracking-[0.4em] whitespace-nowrap">
                                            Autres options éligibles ({eligibleStays.length - 1})
                                        </h3>
                                        <div className="h-0.5 bg-slate-100 flex-1" />
                                    </div>
                                    <div className="grid gap-4">
                                        {eligibleStays.slice(1).map((rule) => {
                                            const details = handleProcedureDetails(userProfile.admin.current_visa_type || 'NONE', rule.id);
                                            const badge = tierBadge(rule);
                                            return (
                                                <div key={rule.id} className="p-6 rounded-2xl border-2 border-slate-100 bg-white hover:border-indigo-200 hover:shadow-lg transition-all duration-200 cursor-pointer">
                                                    <div className="flex flex-wrap items-center gap-3 mb-3">
                                                        <span className={`px-3 py-1 rounded-lg text-[10px] font-bold uppercase tracking-wide ${details.color}`}>
                                                            {details.label}
                                                        </span>
                                                        <span className={`px-2.5 py-1 rounded-md text-[10px] font-bold border ${badge.cls}`}>
                                                            {badge.label}
                                                        </span>
                                                        <span className="px-2.5 py-1 rounded-md text-[10px] font-bold bg-slate-50 text-slate-500 border border-slate-100">
                                                            {durationLabel(rule)}
                                                        </span>
                                                        {rule.gives_work_right && (
                                                            <span className="text-[10px] font-bold text-green-600">💼 Travail</span>
                                                        )}
                                                        {rule.leads_to_naturalisation && (
                                                            <span className="text-[10px] font-bold text-blue-600">🇫🇷 → Naturalisation</span>
                                                        )}
                                                    </div>
                                                    <h5 className="text-lg font-bold text-slate-900">{rule.name}</h5>
                                                    <p className="text-sm text-slate-500 mt-1 line-clamp-2">{rule.description}</p>
                                                </div>
                                            );
                                        })}
                                    </div>
                                </div>
                            )}

                            {/* ─── Parcours d'évolution (upgrade path) ─── */}
                            {eligibleUpgrades.length > 0 && (
                                <div className="mt-12">
                                    <div className="flex items-center gap-6 mb-6">
                                        <h3 className="text-xs font-black text-violet-500 uppercase tracking-[0.4em] whitespace-nowrap">
                                            📈 Votre parcours d'évolution ({eligibleUpgrades.length})
                                        </h3>
                                        <div className="h-0.5 bg-violet-100 flex-1" />
                                    </div>
                                    <p className="text-sm text-slate-500 mb-6">
                                        Avec votre titre actuel et votre durée de résidence, vous pouvez passer au titre supérieur :
                                    </p>
                                    <div className="grid gap-4">
                                        {eligibleUpgrades.map((rule, idx) => {
                                            const badge = tierBadge(rule);
                                            return (
                                                <div key={rule.id} className="relative p-6 rounded-2xl border-2 border-violet-100 bg-gradient-to-r from-violet-50/50 to-white hover:border-violet-300 hover:shadow-lg transition-all duration-200 cursor-pointer">
                                                    {/* Timeline dot */}
                                                    <div className="absolute -left-3 top-8 w-6 h-6 rounded-full bg-violet-500 border-4 border-white shadow-md flex items-center justify-center">
                                                        <span className="text-white text-[8px] font-black">{idx + 1}</span>
                                                    </div>
                                                    <div className="flex flex-wrap items-center gap-3 mb-3 ml-4">
                                                        <span className="px-3 py-1 rounded-lg text-[10px] font-bold bg-violet-100 text-violet-700 border border-violet-200 uppercase tracking-wide">
                                                            🔄 Renouvellement / Upgrade
                                                        </span>
                                                        <span className={`px-2.5 py-1 rounded-md text-[10px] font-bold border ${badge.cls}`}>
                                                            {badge.label}
                                                        </span>
                                                        <span className="px-2.5 py-1 rounded-md text-[10px] font-bold bg-slate-50 text-slate-500 border border-slate-100">
                                                            {durationLabel(rule)}
                                                        </span>
                                                        {rule.gives_work_right && (
                                                            <span className="text-[10px] font-bold text-green-600">💼 Travail</span>
                                                        )}
                                                        {rule.leads_to_naturalisation && (
                                                            <span className="text-[10px] font-bold text-blue-600">🇫🇷 → Naturalisation</span>
                                                        )}
                                                    </div>
                                                    <h5 className="text-lg font-bold text-slate-900 ml-4">{rule.name}</h5>
                                                    <p className="text-sm text-slate-500 mt-1 ml-4 line-clamp-2">{rule.description}</p>
                                                    <p className="text-xs text-violet-400 mt-2 ml-4 font-medium">Réf. : {rule.source_ref}</p>
                                                </div>
                                            );
                                        })}
                                    </div>
                                </div>
                            )}
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

            {/* ─── Legal disclaimer ─── */}
            <div className="mt-12 p-6 bg-amber-50/70 border-2 border-amber-100 rounded-2xl">
                <div className="flex gap-3 items-start">
                    <span className="text-xl">⚖️</span>
                    <div className="space-y-2">
                        <h4 className="text-sm font-bold text-amber-900">Information importante</h4>
                        <p className="text-xs text-amber-800 leading-relaxed">
                            Les résultats de ce simulateur n'ont <strong>aucune valeur juridique</strong>. Ils constituent une
                            indication préliminaire d'éligibilité basée sur les informations que vous avez renseignées et les
                            textes du Code de l'entrée et du séjour des étrangers (CESEDA). Seule une consultation avec un
                            professionnel du droit des étrangers peut confirmer votre éligibilité. Chaque dossier est unique
                            et peut dépendre de circonstances non couvertes par cet outil.
                        </p>
                    </div>
                </div>
            </div>

            {/* ─── Recap of user answers ─── */}
            {(() => {
                const nationalityLabels: Record<string, string> = {
                    EU: '🇪🇺 UE / EEE / Suisse', ALGERIAN: '🇩🇿 Algérienne', TUNISIAN: '🇹🇳 Tunisienne',
                    MOROCCAN: '🇲🇦 Marocaine', NON_EU: '🌍 Non-UE', FRANCE: '🇫🇷 Française',
                    REFUGEE: '🛡️ Réfugié / Protection', STATELESS: '🌐 Apatride',
                };
                const visaLabels: Record<string, string> = {
                    'VLS-TS': 'Visa long séjour (VLS-TS)', STUDENT: 'Titre étudiant', WORKER: 'Titre salarié',
                    VPF: 'Vie Privée et Familiale', VISITOR: 'Visiteur', PASSEPORT_TALENT: 'Passeport Talent',
                    RESIDENT_CARD: 'Carte de Résident', RECEIPISSE: 'Récépissé', NONE: 'Aucun titre',
                };
                const contractLabels: Record<string, string> = { CDI: 'CDI', CDD: 'CDD', SEASONAL: 'Saisonnier', NONE: 'Aucun', PROMESSE: 'Promesse d\'embauche' };
                const situationLabels: Record<string, string> = { STUDENT: 'Étudiant', WORKER: 'Salarié', ENTREPRENEUR: 'Indépendant', OTHER: 'Autre' };
                const goalLabels: Record<string, string> = { RESIDENCE_PERMIT: 'Titre de Séjour', NATURALIZATION: 'Naturalisation', BOTH: 'Les deux', SERVICE: 'Service' };
                const diplomaLabels: Record<string, string> = { NONE: 'Aucun', LICENCE: 'Licence', MASTER: 'Master', PHD: 'Doctorat', SPECIALIZED_MASTER: 'Master spécialisé', LICENCE_PRO: 'Licence pro', CGE_LEVEL_1: 'Grande école (niveau 1)' };

                type RecapLine = { label: string; value: string };
                type RecapGroup = { title: string; icon: string; lines: RecapLine[] };

                const boolStr = (v: boolean | undefined) => v ? 'Oui' : 'Non';
                const p = userProfile;

                const groups: RecapGroup[] = [
                    {
                        title: 'Identité', icon: '👤', lines: [
                            { label: 'Objectif', value: goalLabels[p.project.target_goal || ''] || '—' },
                            { label: 'Nationalité', value: nationalityLabels[p.identity.nationality_group] || p.identity.nationality_group },
                            { label: 'Âge', value: p.identity.age ? `${p.identity.age} ans` : '—' },
                            { label: 'Né(e) en France', value: boolStr(p.identity.born_in_france) },
                        ].filter(l => l.value && l.value !== '—'),
                    },
                    {
                        title: 'Administratif', icon: '📋', lines: [
                            { label: 'Titre actuel', value: visaLabels[p.admin.current_visa_type || ''] || '—' },
                            ...(p.timeline.entry_date ? [{ label: 'Date d\'entrée', value: new Date(p.timeline.entry_date).toLocaleDateString('fr-FR') }] : []),
                            ...(p.timeline.years_continuous_residence > 0 ? [{ label: 'Résidence continue', value: `${p.timeline.years_continuous_residence} ans` }] : []),
                            ...(p.admin.entered_legally !== undefined ? [{ label: 'Entrée régulière', value: boolStr(p.admin.entered_legally) }] : []),
                        ].filter(l => l.value && l.value !== '—'),
                    },
                    {
                        title: 'Travail', icon: '💼', lines: [
                            ...(p.work.main_situation ? [{ label: 'Situation principale', value: situationLabels[p.work.main_situation] || p.work.main_situation }] : []),
                            ...(p.work.contract_type && p.work.contract_type !== 'NONE' ? [{ label: 'Contrat', value: contractLabels[p.work.contract_type] || p.work.contract_type }] : []),
                            ...(p.work.salary_monthly_gross > 0 ? [{ label: 'Salaire brut mensuel', value: `${p.work.salary_monthly_gross.toLocaleString('fr-FR')} €` }] : []),
                            ...(p.work.contract_duration_months ? [{ label: 'Durée du contrat', value: `${p.work.contract_duration_months} mois` }] : []),
                            { label: 'Autorisation de travail', value: boolStr(p.work.has_work_authorization) },
                            { label: 'Métier en tension', value: boolStr(p.work.job_in_tension_list) },
                        ].filter(l => l.value && l.value !== '—'),
                    },
                    {
                        title: 'Études', icon: '🎓', lines: [
                            ...(p.education.diploma_level && p.education.diploma_level !== 'NONE' ? [{ label: 'Diplôme', value: diplomaLabels[p.education.diploma_level] || p.education.diploma_level }] : []),
                            ...(p.education.is_enrolled_higher_ed ? [{ label: 'Inscrit en supérieur', value: boolStr(p.education.is_enrolled_higher_ed) }] : []),
                            ...(p.education.years_schooling_france ? [{ label: 'Scolarité en France', value: `${p.education.years_schooling_france} ans` }] : []),
                        ],
                    },
                    {
                        title: 'Intégration', icon: '🗣️', lines: [
                            { label: 'Niveau de français', value: p.integration.french_level || '—' },
                            { label: 'Examen civique réussi', value: boolStr(p.integration.civic_exam_passed) },
                            { label: 'Adhésion valeurs républicaines', value: boolStr(p.integration.adheres_to_republican_values) },
                        ].filter(l => l.value && l.value !== '—'),
                    },
                    {
                        title: 'Famille', icon: '💍', lines: [
                            { label: 'Conjoint', value: p.family.spouse_nationality === 'NONE' ? 'Célibataire' : p.family.spouse_nationality === 'FRENCH' ? 'Conjoint français' : p.family.spouse_nationality === 'EU' ? 'Conjoint UE' : 'Conjoint non-UE' },
                            ...(p.family.marriage_duration_years > 0 ? [{ label: 'Durée du mariage', value: `${p.family.marriage_duration_years} ans` }] : []),
                            { label: 'Enfant français', value: boolStr(p.family.has_french_child) },
                            ...(p.family.is_pacsed_with_french ? [{ label: 'Pacsé(e) avec un(e) Français(e)', value: boolStr(p.family.is_pacsed_with_french) }] : []),
                        ],
                    },
                    {
                        title: 'Civique', icon: '⚖️', lines: [
                            { label: 'Casier vierge', value: boolStr(p.civic.clean_criminal_record) },
                            { label: 'Pas de mesure d\'éloignement', value: boolStr(p.civic.no_expulsion_order) },
                            { label: 'Couverture maladie', value: boolStr(p.admin.health_insurance) },
                        ],
                    },
                    {
                        title: 'Vulnérabilité', icon: '🩺', lines: [
                            ...(p.vulnerability.is_victim_domestic_violence ? [{ label: 'Victime de violences', value: 'Oui' }] : []),
                            ...(p.vulnerability.is_victim_trafficking ? [{ label: 'Victime de traite', value: 'Oui' }] : []),
                            ...(p.vulnerability.has_protection_order_violence ? [{ label: 'Ordonnance de protection', value: 'Oui' }] : []),
                            ...(p.health.personal_needs_treatment ? [{ label: 'Besoin de soins médicaux', value: 'Oui' }] : []),
                        ],
                    },
                    {
                        title: 'Régularisation', icon: '📋', lines: [
                            ...(p.regularisation.has_children_schooled_3y ? [{ label: 'Enfants scolarisés 3+ ans', value: 'Oui' }] : []),
                            ...(p.regularisation.has_exceptional_talent ? [{ label: 'Talent exceptionnel', value: 'Oui' }] : []),
                        ],
                    },
                ].filter(g => g.lines.length > 0);

                return (
                    <details className="mt-8 group">
                        <summary className="cursor-pointer select-none flex items-center gap-3 p-4 bg-slate-50 hover:bg-slate-100 rounded-2xl transition-colors">
                            <span className="text-lg">📝</span>
                            <span className="text-sm font-bold text-slate-700 flex-1">Récapitulatif de vos réponses</span>
                            <span className="text-xs text-slate-400 group-open:hidden">Cliquez pour afficher ▸</span>
                            <span className="text-xs text-slate-400 hidden group-open:inline">▾ Masquer</span>
                        </summary>

                        <div className="mt-4 grid grid-cols-1 md:grid-cols-2 gap-4">
                            {groups.map(g => (
                                <div key={g.title} className="p-4 bg-white border border-slate-100 rounded-xl shadow-sm">
                                    <h5 className="text-xs font-bold uppercase tracking-wider text-slate-500 mb-3 flex items-center gap-2">
                                        <span>{g.icon}</span> {g.title}
                                    </h5>
                                    <dl className="space-y-1.5">
                                        {g.lines.map(line => (
                                            <div key={line.label} className="flex items-baseline justify-between gap-2 text-xs">
                                                <dt className="text-slate-500 shrink-0">{line.label}</dt>
                                                <dd className="text-slate-800 font-medium text-right">{line.value}</dd>
                                            </div>
                                        ))}
                                    </dl>
                                </div>
                            ))}
                        </div>
                    </details>
                );
            })()}

            <div className="mt-auto pt-6 border-t border-slate-100 opacity-50">
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

            {/* LEAD GENERATION & CHECKOUT MODAL */}
            {showLeadModal && (
                <div className="fixed inset-0 z-[100] flex items-center justify-center p-6 backdrop-blur-xl bg-slate-900/40 animate-in fade-in duration-300">
                    <div className="bg-white w-full max-w-xl rounded-[3.5rem] shadow-2xl relative overflow-hidden animate-in zoom-in-95 slide-in-from-bottom-10 duration-500 max-h-[90vh] overflow-y-auto">
                        <button
                            onClick={() => {
                                setShowLeadModal(false);
                                setIsProcessing(false);
                                setPendingAction(null);
                            }}
                            className="absolute top-6 right-6 p-3 hover:bg-slate-100 rounded-full transition-colors text-slate-400 hover:text-slate-900 z-50 bg-white/80 backdrop-blur"
                        >
                            <XCircle className="w-6 h-6" />
                        </button>

                        <div className="p-8 md:p-12 space-y-8">
                            <div className="space-y-4 text-center">
                                <div className={cn(
                                    "w-16 h-16 rounded-2xl flex items-center justify-center mb-4 mx-auto",
                                    pendingAction ? "bg-indigo-50 text-indigo-600" : "bg-emerald-50 text-emerald-600"
                                )}>
                                    {pendingAction ? <Scale className="w-8 h-8" /> : <Smartphone className="w-8 h-8" />}
                                </div>
                                <h2 className="text-3xl font-black text-slate-900 leading-tight">
                                    {pendingAction ? "Validation de la commande" : "Demande de rappel"}
                                </h2>
                                {!pendingAction && (
                                    <p className="text-slate-500 font-medium">
                                        Un conseiller spécialisé vous contactera sous 24h.
                                    </p>
                                )}
                            </div>

                            {/* RECAPITULATIF COMMANDE (Si Paiement) */}
                            {pendingAction && (
                                <div className="bg-slate-50 border border-slate-200 rounded-2xl p-6 space-y-4">
                                    <h3 className="text-xs font-black text-slate-400 uppercase tracking-widest border-b border-slate-200 pb-2">Récapitulatif</h3>
                                    <div className="flex justify-between items-start">
                                        <div>
                                            <p className="font-bold text-slate-900 text-lg">{pendingAction.label}</p>
                                            <p className="text-sm text-slate-500">Service d'accompagnement juridique</p>
                                        </div>
                                        <p className="font-black text-xl text-indigo-600">{pendingAction.amount} €</p>
                                    </div>
                                    <div className="flex gap-2 items-start text-xs text-slate-500 bg-white p-3 rounded-lg border border-slate-100">
                                        <Info className="w-4 h-4 shrink-0 text-indigo-500" />
                                        <p>Ce tarif inclut l'analyse du dossier, le suivi administratif et l'accès à votre espace client sécurisé.</p>
                                    </div>
                                </div>
                            )}

                            <div className="space-y-6">
                                <div className="grid md:grid-cols-2 gap-4">
                                    <div className="space-y-2">
                                        <label className="text-xs font-black text-slate-400 uppercase tracking-widest ml-2">Nom Complet</label>
                                        <input
                                            type="text"
                                            value={leadForm.name}
                                            onChange={(e) => setLeadForm({ ...leadForm, name: e.target.value })}
                                            placeholder="Jean Dupont"
                                            className="w-full bg-slate-50 border-2 border-slate-100 rounded-xl px-4 py-3 font-bold focus:outline-none focus:border-indigo-600 focus:bg-white transition-all"
                                        />
                                    </div>
                                    <div className="space-y-2">
                                        <label className="text-xs font-black text-slate-400 uppercase tracking-widest ml-2">Téléphone</label>
                                        <input
                                            type="tel"
                                            value={leadForm.phone}
                                            onChange={(e) => setLeadForm({ ...leadForm, phone: e.target.value })}
                                            placeholder="06 12 34 56 78"
                                            className="w-full bg-slate-50 border-2 border-slate-100 rounded-xl px-4 py-3 font-bold focus:outline-none focus:border-indigo-600 focus:bg-white transition-all"
                                        />
                                    </div>
                                </div>

                                {/* SECTION LEGALE (Uniquement si Paiement) */}
                                {pendingAction && (
                                    <div className="space-y-4 pt-4 border-t border-slate-100">
                                        <label className="flex items-start gap-3 p-3 hover:bg-slate-50 rounded-xl cursor-pointer transition-colors group">
                                            <input type="checkbox" className="mt-1 w-5 h-5 rounded border-slate-300 text-indigo-600 focus:ring-indigo-500" id="accept-cgu" />
                                            <span className="text-sm text-slate-600 leading-snug">
                                                Je reconnais avoir lu et accepté les <a href="#" className="font-bold underline decoration-slate-300 underline-offset-2 hover:text-indigo-600">Conditions Générales de Vente (CGV)</a> et la Politique de Confidentialité.
                                            </span>
                                        </label>

                                        <label className="flex items-start gap-3 p-3 hover:bg-slate-50 rounded-xl cursor-pointer transition-colors group">
                                            <input type="checkbox" className="mt-1 w-5 h-5 rounded border-slate-300 text-indigo-600 focus:ring-indigo-500" id="accept-mandate" />
                                            <span className="text-sm text-slate-600 leading-snug">
                                                <strong>Mandat Express :</strong> Je donne mandat à Simulegal (SAS) pour effectuer en mon nom les démarches de pré-qualification et de constitution de dossier relatives au service commandé.
                                            </span>
                                        </label>
                                    </div>
                                )}

                                <button
                                    onClick={() => {
                                        // Basic Client-Side Validation required for checkboxes if pendingAction
                                        if (pendingAction) {
                                            const cgu = document.getElementById('accept-cgu') as HTMLInputElement;
                                            const mand = document.getElementById('accept-mandate') as HTMLInputElement;
                                            if (!cgu?.checked || !mand?.checked) {
                                                alert("Veuillez accepter les conditions générales et le mandat pour continuer.");
                                                return;
                                            }
                                            performCheckout({ name: leadForm.name, phone: leadForm.phone });
                                        } else {
                                            performCheckout({ name: leadForm.name, phone: leadForm.phone });
                                            alert(`Merci ${leadForm.name} ! Un conseiller va vous rappeler au ${leadForm.phone}.`);
                                            setShowLeadModal(false);
                                        }
                                    }}
                                    disabled={!leadForm.name || !leadForm.phone || isProcessing}
                                    className={cn(
                                        "w-full py-6 text-white text-xl font-black rounded-2xl hover:scale-[1.02] transition-all shadow-xl disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:scale-100 flex justify-center items-center gap-3",
                                        pendingAction ? "bg-indigo-600 hover:bg-indigo-700 shadow-indigo-200" : "bg-slate-900 hover:bg-black"
                                    )}
                                >
                                    {isProcessing && <div className="w-5 h-5 border-2 border-white/30 border-t-white rounded-full animate-spin"></div>}
                                    {pendingAction ? "Signer le mandat & Payer" : "Valider ma demande"}
                                </button>

                                {pendingAction && (
                                    <p className="text-center text-xs text-slate-400 font-medium">
                                        En cliquant sur ce bouton, vous validez votre commande avec obligation de paiement.
                                    </p>
                                )}
                            </div>
                        </div>

                        <div className="bg-slate-50 py-4 text-center text-[10px] font-black text-slate-400 uppercase tracking-[0.4em]">
                            Simulegal Secure Checkout
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
