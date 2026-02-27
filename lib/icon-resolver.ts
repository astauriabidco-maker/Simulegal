/**
 * Résolveur d'icônes centralisé — Source unique pour toute la plateforme.
 * 
 * Usage :
 *   import { resolveIcon } from '@/lib/icon-resolver';
 *   const Icon = resolveIcon('Flag');
 *   <Icon className="w-5 h-5" />
 */

import {
    Flag,
    FileText,
    ShieldCheck,
    GraduationCap,
    Languages,
    BookOpen,
    Scale,
    Users,
    Briefcase,
    Car,
    Calendar,
    Gavel,
    Phone,
    Heart,
    Home,
    Globe,
    Award,
    Building,
    Baby,
    Plane,
    Landmark,
    HandCoins,
    Stethoscope,
    Lightbulb,
    Megaphone,
    Handshake,
    CircleDollarSign,
    Stamp,
    ScrollText,
    SquareUserRound,
    MapPin,
    ClipboardList,
    Wallet,
    type LucideIcon,
} from 'lucide-react';

/**
 * Registre central des icônes disponibles.
 * Pour ajouter une nouvelle icône :
 *   1. Importer depuis lucide-react ci-dessus
 *   2. Ajouter l'entrée dans ICON_REGISTRY
 */
const ICON_REGISTRY: Record<string, LucideIcon> = {
    // Procédures & Juridique
    Flag,
    FileText,
    ShieldCheck,
    Scale,
    Gavel,
    Stamp,
    ScrollText,
    ClipboardList,
    Landmark,

    // Personnes & Famille
    Users,
    Baby,
    Heart,
    SquareUserRound,

    // Éducation & Formation
    GraduationCap,
    Languages,
    BookOpen,
    Lightbulb,

    // Transport & Logement
    Car,
    Plane,
    Home,
    MapPin,

    // Travail & Finance
    Briefcase,
    Building,
    HandCoins,
    CircleDollarSign,
    Wallet,

    // Communication & Santé
    Phone,
    Calendar,
    Megaphone,
    Handshake,
    Stethoscope,

    // Divers
    Globe,
    Award,
};

/**
 * Résout une icône par son nom. Retourne FileText comme fallback.
 */
export function resolveIcon(iconName: string): LucideIcon {
    return ICON_REGISTRY[iconName] || FileText;
}

/**
 * Retourne la liste de toutes les icônes disponibles (pour sélecteur admin).
 */
export function getAvailableIcons(): { name: string; icon: LucideIcon }[] {
    return Object.entries(ICON_REGISTRY).map(([name, icon]) => ({ name, icon }));
}

export type { LucideIcon };
