'use client';

import React, { useState } from 'react';
import SimulatorWrapper from '@/components/SimulatorWrapper';

export default function Home() {
  const [showSimulator, setShowSimulator] = useState(false);

  if (showSimulator) {
    return (
      <main className="min-h-screen bg-gradient-to-b from-slate-50 to-slate-100 py-12">
        <div className="max-w-4xl mx-auto px-4 mb-8 flex justify-between items-center">
          <div className="flex items-center gap-2">
            <div className="w-8 h-8 bg-indigo-600 rounded-lg flex items-center justify-center text-white font-bold">S</div>
            <span className="text-xl font-bold text-slate-900 tracking-tight">SimuLegal</span>
          </div>
          <button
            onClick={() => setShowSimulator(false)}
            className="text-sm font-medium text-slate-500 hover:text-indigo-600 transition-colors"
          >
            Retour à l'accueil
          </button>
        </div>
        <SimulatorWrapper />
      </main>
    );
  }

  return (
    <div className="min-h-screen bg-white font-sans text-slate-900 selection:bg-indigo-100 selection:text-indigo-900">
      {/* Navigation */}
      <nav className="fixed top-0 w-full z-50 bg-white/80 backdrop-blur-md border-b border-slate-100">
        <div className="max-w-7xl mx-auto px-6 h-20 flex justify-between items-center">
          <div className="flex items-center gap-2">
            <div className="w-10 h-10 bg-indigo-600 rounded-xl flex items-center justify-center text-white font-bold text-xl shadow-lg shadow-indigo-200">S</div>
            <span className="text-2xl font-bold tracking-tight">SimuLegal</span>
          </div>
          <div className="hidden md:flex items-center gap-8">
            <a href="#" className="text-sm font-medium text-slate-600 hover:text-indigo-600 transition-colors">Services</a>
            <a href="#" className="text-sm font-medium text-slate-600 hover:text-indigo-600 transition-colors">Tarifs</a>
            <a href="#" className="text-sm font-medium text-slate-600 hover:text-indigo-600 transition-colors">Blog</a>
            <button
              onClick={() => setShowSimulator(true)}
              className="px-6 py-2.5 bg-indigo-600 text-white text-sm font-bold rounded-full hover:bg-indigo-700 transition-all shadow-lg shadow-indigo-200 active:scale-95"
            >
              Lancer le simulateur
            </button>
          </div>
        </div>
      </nav>

      {/* Hero Section */}
      <main className="pt-32 pb-20 overflow-hidden">
        <div className="max-w-7xl mx-auto px-6 grid md:grid-cols-2 gap-16 items-center">
          <div className="relative z-10">
            <div className="inline-flex items-center gap-2 px-3 py-1 bg-indigo-50 text-indigo-700 rounded-full text-xs font-bold uppercase tracking-wider mb-6 border border-indigo-100">
              <span className="flex h-2 w-2 rounded-full bg-indigo-600"></span>
              Mis à jour pour 2024
            </div>
            <h1 className="text-5xl md:text-7xl font-extrabold leading-[1.1] tracking-tight mb-8">
              Votre avenir en France <br />
              <span className="text-transparent bg-clip-text bg-gradient-to-r from-indigo-600 to-indigo-400">commence ici.</span>
            </h1>
            <p className="text-xl text-slate-500 leading-relaxed mb-10 max-w-lg">
              Simulez votre éligibilité à plus de 40 titres de séjour et à la nationalité française en moins de 3 minutes.
            </p>
            <div className="flex flex-col sm:flex-row gap-4 mb-12">
              <button
                onClick={() => setShowSimulator(true)}
                className="px-8 py-4 bg-indigo-600 text-white text-lg font-bold rounded-2xl hover:bg-indigo-700 transition-all shadow-xl shadow-indigo-200 active:scale-95 text-center"
              >
                Tester mon éligibilité
              </button>
              <button className="px-8 py-4 bg-slate-50 text-slate-700 text-lg font-bold rounded-2xl hover:bg-slate-100 transition-all border border-slate-200 text-center">
                Voir toutes les procédures
              </button>
            </div>
            <div className="flex items-center gap-6">
              <div className="flex -space-x-3">
                {[1, 2, 3, 4].map(i => (
                  <div key={i} className="w-10 h-10 rounded-full border-2 border-white bg-slate-200 flex items-center justify-center overflow-hidden">
                    <img src={`https://i.pravatar.cc/100?img=${i + 10}`} alt="user" />
                  </div>
                ))}
              </div>
              <p className="text-sm text-slate-500 font-medium tracking-tight">
                <span className="text-slate-900 font-bold">12,400+</span> simulations terminées ce mois-ci
              </p>
            </div>
          </div>

          <div className="relative">
            <div className="absolute -top-20 -right-20 w-96 h-96 bg-indigo-100 rounded-full blur-3xl opacity-50"></div>
            <div className="absolute -bottom-20 -left-20 w-96 h-96 bg-indigo-50 rounded-full blur-3xl opacity-50"></div>
            <div className="relative bg-white p-8 rounded-[2.5rem] shadow-2xl shadow-indigo-100 border border-slate-100 rotate-2">
              <div className="space-y-6">
                {[
                  { label: 'Identité', status: 'Complété', color: 'bg-emerald-500' },
                  { label: 'Situation Pro', status: 'Complété', color: 'bg-emerald-500' },
                  { label: 'Droit au séjour', status: 'Analyse...', color: 'bg-indigo-600 animate-pulse' },
                ].map((item, idx) => (
                  <div key={idx} className="flex items-center justify-between p-4 bg-slate-50 rounded-2xl border border-slate-100">
                    <div className="flex items-center gap-3">
                      <div className={`w-3 h-3 rounded-full ${item.color}`}></div>
                      <span className="font-bold text-slate-700">{item.label}</span>
                    </div>
                    <span className="text-xs font-bold text-slate-400 uppercase tracking-widest">{item.status}</span>
                  </div>
                ))}
                <div className="pt-4 mt-4 border-t border-slate-100">
                  <div className="p-6 bg-indigo-600 rounded-2xl text-white shadow-lg shadow-indigo-200">
                    <h4 className="font-bold mb-1">Meilleur résultat :</h4>
                    <p className="text-2xl font-black">Passeport Talent</p>
                    <div className="mt-4 flex items-center justify-between">
                      <span className="text-xs text-indigo-100 font-medium">Confiance : 98.4%</span>
                      <div className="w-20 h-1 bg-white/20 rounded-full overflow-hidden">
                        <div className="w-[98%] h-full bg-white"></div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </main>

      {/* Trust Section */}
      <section className="bg-slate-50 py-20">
        <div className="max-w-7xl mx-auto px-6 text-center">
          <p className="text-sm font-bold text-slate-400 uppercase tracking-widest mb-10">Conçu avec les dernières lois en vigueur</p>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-12 opacity-50 grayscale hover:grayscale-0 transition-all duration-500">
            <span className="text-xl font-bold italic">CESEDA 2024</span>
            <span className="text-xl font-bold italic">LOI IMMIGRATION</span>
            <span className="text-xl font-bold italic">SERVICE PUBLIC</span>
            <span className="text-xl font-bold italic">MINISTÈRE DE L'INTÉRIEUR</span>
          </div>
        </div>
      </section>
    </div>
  );
}
