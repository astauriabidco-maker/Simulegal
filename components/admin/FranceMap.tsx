'use client';

import React, { useState } from 'react';
import { MapPin, Building2, Store } from 'lucide-react';
import { Agency } from '../../types/backoffice';

// Simplified SVG paths (stylized for dashboard)
const REGION_PATHS = [
    { id: 'HDF', label: 'Hauts-de-France', d: "M200 20 L240 40 L240 80 L180 80 L160 50 Z", color: "#ec4899", dot: { x: 200, y: 50 } },
    { id: 'NOR', label: 'Normandie', d: "M160 50 L180 80 L140 100 L80 100 L100 60 Z", color: "#f97316", dot: { x: 120, y: 80 } },
    { id: 'IDF', label: 'Île-de-France', d: "M180 80 L220 80 L220 120 L180 120 Z", color: "#6366f1", dot: { x: 200, y: 100 } },
    { id: 'GES', label: 'Grand Est', d: "M240 40 L320 60 L320 120 L280 140 L220 120 L240 80 Z", color: "#06b6d4", dot: { x: 280, y: 90 } },
    { id: 'BRE', label: 'Bretagne', d: "M80 100 L100 140 L40 160 L20 120 L50 100 Z", color: "#0ea5e9", dot: { x: 60, y: 130 } },
    { id: 'PDL', label: 'Pays de la Loire', d: "M100 140 L140 140 L140 180 L80 180 L80 150 Z", color: "#84cc16", dot: { x: 110, y: 160 } },
    { id: 'CVL', label: 'Centre-Val de Loire', d: "M140 140 L220 120 L220 180 L170 200 L140 180 Z", color: "#14b8a6", dot: { x: 180, y: 160 } },
    { id: 'BFC', label: 'Bourgogne-Franche-Comté', d: "M240 120 L280 140 L300 180 L260 220 L220 180 Z", color: "#d946ef", dot: { x: 260, y: 170 } },
    { id: 'NAQ', label: 'Nouvelle-Aquitaine', d: "M80 180 L140 180 L170 200 L170 280 L100 320 L50 280 L50 200 Z", color: "#ef4444", dot: { x: 100, y: 240 } },
    { id: 'AURA', label: 'Auvergne-Rhône-Alpes', d: "M220 180 L260 220 L280 260 L240 300 L180 300 L170 200 Z", color: "#10b981", dot: { x: 230, y: 250 } },
    { id: 'OCC', label: 'Occitanie', d: "M100 320 L170 280 L180 300 L240 300 L200 360 L120 360 Z", color: "#8b5cf6", dot: { x: 160, y: 320 } },
    { id: 'PACA', label: 'Provence-Alpes-Côte d\'Azur', d: "M240 300 L300 300 L320 340 L260 360 Z", color: "#f59e0b", dot: { x: 280, y: 330 } },
    { id: 'COR', label: 'Corse', d: "M320 380 L340 380 L340 420 L320 420 Z", color: "#fbbf24", dot: { x: 330, y: 400 } },
];

interface FranceMapProps {
    agencies: Agency[];
    onAgencyClick?: (agency: Agency) => void;
}

export default function FranceMap({ agencies, onAgencyClick }: FranceMapProps) {
    const [hoveredRegion, setHoveredRegion] = useState<string | null>(null);

    const getAgenciesInRegion = (regionId: string) => {
        return agencies.filter(a => a.region === regionId);
    };

    return (
        <div className="relative w-full aspect-square max-w-[500px] mx-auto bg-white rounded-[3rem] p-8 shadow-inner border border-slate-100 overflow-hidden group/map">
            {/* Background pattern */}
            <div className="absolute inset-0 opacity-[0.03] pointer-events-none" style={{ backgroundImage: 'radial-gradient(#6366f1 1px, transparent 1px)', backgroundSize: '20px 20px' }} />

            <svg viewBox="0 0 400 450" className="w-full h-full drop-shadow-2xl">
                <g>
                    {REGION_PATHS.map((region) => {
                        const regionAgencies = getAgenciesInRegion(region.id);
                        const isHovered = hoveredRegion === region.id;

                        return (
                            <g
                                key={region.id}
                                onMouseEnter={() => setHoveredRegion(region.id)}
                                onMouseLeave={() => setHoveredRegion(null)}
                                className="cursor-pointer transition-all duration-300"
                            >
                                <path
                                    d={region.d}
                                    fill={isHovered ? region.color : '#f1f5f9'}
                                    stroke={isHovered ? 'white' : '#e2e8f0'}
                                    strokeWidth={isHovered ? 3 : 1}
                                    className="transition-all duration-500 ease-out"
                                />

                                {regionAgencies.length > 0 && (
                                    <g transform={`translate(${region.dot.x}, ${region.dot.y})`}>
                                        <circle
                                            r={isHovered ? 12 : 8}
                                            fill={isHovered ? 'white' : region.color}
                                            className="animate-pulse shadow-lg"
                                        />
                                        <text
                                            y={2}
                                            textAnchor="middle"
                                            fill={isHovered ? region.color : 'white'}
                                            fontSize={isHovered ? "10" : "8"}
                                            fontWeight="black"
                                            className="pointer-events-none"
                                        >
                                            {regionAgencies.length}
                                        </text>
                                    </g>
                                )}
                            </g>
                        );
                    })}
                </g>
            </svg>

            {/* Floating Info Card */}
            {hoveredRegion && (
                <div className="absolute top-8 left-8 bg-white/90 backdrop-blur-md p-4 rounded-3xl border border-slate-100 shadow-xl animate-in fade-in slide-in-from-left-4 duration-300 z-20">
                    <p className="text-[10px] font-black uppercase text-slate-400 tracking-widest mb-1">Région</p>
                    <p className="font-black text-slate-900 text-lg">
                        {REGION_PATHS.find(r => r.id === hoveredRegion)?.label}
                    </p>
                    <div className="mt-2 flex items-center gap-2">
                        <div className="w-8 h-8 bg-indigo-50 text-indigo-600 rounded-lg flex items-center justify-center">
                            <Store size={16} />
                        </div>
                        <p className="text-sm font-bold text-slate-600">
                            {getAgenciesInRegion(hoveredRegion).length} Agences actives
                        </p>
                    </div>
                </div>
            )}

            {/* Legend */}
            <div className="absolute bottom-8 right-8 flex flex-col gap-2 items-end">
                <div className="flex items-center gap-2 bg-white/50 backdrop-blur-sm px-3 py-1.5 rounded-full border border-slate-200">
                    <div className="w-2 h-2 rounded-full bg-indigo-500" />
                    <span className="text-[10px] font-black uppercase tracking-tighter text-slate-600">Implantation Active</span>
                </div>
            </div>
        </div>
    );
}
