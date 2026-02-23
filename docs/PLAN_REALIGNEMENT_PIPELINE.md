# 📋 Plan de Réalignement — Module Pipeline de Vente

> **Objectif :** Transformer le pipeline d'un flux de "conversion en ligne" vers un flux
> d'**obtention de rendez-vous en agence** pour dérouler le simulateur d'éligibilité en face-à-face.

---

## 🔄 Le Vrai Parcours Client

### Sources des leads (entrantes)
Les leads **ne viennent PAS de la Landing Page**. La landing page est un canal self-service
où l'utilisateur final fait lui-même son diagnostic d'éligibilité.

Les leads du pipeline arrivent de **3 canaux externes** :
- **🤝 Partenaires via API** — Injection directe par des partenaires (associations, cabinets, etc.)
- **🌐 Site web** — Formulaires de contact, demandes de rappel, demandes d'info
- **⚡ Webhooks** — Meta Ads Lead Forms, Google Ads, TikTok Ads, etc.

### Flux du pipeline

```
SOURCES EXTERNES                 PIPELINE DE VENTE                    EN AGENCE
────────────────                 ─────────────────                    ──────────

🤝 Partenaire API  ──┐                                              
🌐 Site web         ──┼──►  Lead créé (À Appeler)                   
⚡ Webhook (Ads)    ──┘          │                                   
                                 ▼                                   
                          Commercial appelle                         
                          le lead pour qualifier                     
                                 │                                   
                          Confirme le besoin                        
                          au téléphone                               
                                 │                                   
                          Propose un RDV                             
                          en agence                                  
                                 │                                   
                                 ▼                                   
                          RDV Fixé              ──►  Lead vient en agence
                          (date + agence)                  │
                                                    Dérouler le Simulateur
                                                    Vérifier éligibilité
                                                           │
                                                    Si éligible → Signer
                                                    Ouverture dossier
                                                    (Paiement + Collecte docs)
```

---

## 📊 Phase 1 : Refonte des Colonnes Kanban

### Avant (actuel)
| Colonne | Problème |
|---------|----------|
| À Appeler | ✅ OK |
| En Discussion | ✅ OK |
| RDV Fixé | ✅ OK mais manque la date/agence |
| **Lien Envoyé** | ❌ Hors sujet — pas de lien à envoyer |
| **Converti** | ❌ La conversion se fait en agence, pas ici |
| Perdu | ✅ OK |

### Après (réaligné)
| Colonne | Description | Action du commercial |
|---------|-------------|---------------------|
| 🟡 **À Appeler** | Lead reçu depuis landing/pub. En attente de premier contact. | Appeler, qualifier le besoin |
| 🟣 **En Discussion** | Appel passé, besoin confirmé. En cours de planification RDV. | Proposer un créneau en agence |
| 🔵 **RDV Fixé** | RDV confirmé avec date, heure et agence. | Confirmer par WhatsApp/SMS |
| 🟢 **RDV Effectué** | Lead venu en agence. Simulateur déroulé. | Valider l'éligibilité, proposer le service |
| ✅ **Signé** | Contrat signé, dossier ouvert dans le CRM. | → Transfert vers "Dossiers Clients" |
| ⬛ **Perdu** | Lead injoignable, non intéressé ou non éligible. | Archiver avec motif |

### Fichiers à modifier
- `components/sales/SalesDashboard.tsx` → Constante `COLUMNS` (ligne 29-36)
- `services/SalesStore.ts` → Type `ProspectStatus` (ligne 6)
- `backend/src/sales/sales.service.ts` → Enum statuts si applicable

---

## 📅 Phase 2 : RDV avec Date + Agence

Actuellement, "RDV Fixé" est juste un statut. Il faut stocker les **infos du RDV**.

### Ajouter au modèle `Prospect`
```typescript
// Dans services/SalesStore.ts → interface Prospect
appointmentDate?: string;        // ISO date-heure du RDV
appointmentAgencyId?: string;    // ID de l'agence
appointmentAgencyName?: string;  // Nom lisible ("Agence Paris 15")
appointmentConfirmed?: boolean;  // Confirmé par le lead ?
```

### Créer un modal "Fixer un RDV"
Au lieu du bouton "Convertir en dossier client", le CTA principal doit être **"Fixer un RDV"** :
- Sélecteur de date/heure (DatePicker)
- Sélecteur d'agence (liste des agences du réseau)
- Service concerné (pré-rempli depuis `interestServiceId`)
- Option d'envoyer la confirmation par SMS/WhatsApp

### Fichiers à créer/modifier
- **CRÉER** `components/sales/BookAppointmentModal.tsx`
- **MODIFIER** `components/sales/SalesDashboard.tsx` → remplacer `handleConvert` par `handleBookAppointment`
- **MODIFIER** `services/SalesStore.ts` → ajouter `bookAppointment()` dans le store

---

## 📞 Phase 3 : Refonte de la Fiche Prospect (Drawer)

### Adapter les CTA à chaque étape

| Statut du prospect | CTA principal | Actions secondaires |
|-------------------|---------------|---------------------|
| **À Appeler** | 📞 **Appeler** | WhatsApp, Ajouter note |
| **En Discussion** | 📅 **Fixer un RDV** | Appeler, WhatsApp, Ajouter note |
| **RDV Fixé** | ✅ **Confirmer présence** | Rappeler, Modifier RDV, Annuler |
| **RDV Effectué** | 🔬 **Ouvrir Simulateur** | Convertir en dossier, Ajouter note |
| **Signé** | 📂 **Voir dossier CRM** | — |
| **Perdu** | 🔄 **Réactiver** | — |

### Afficher les infos RDV dans le drawer
Quand le prospect a un RDV fixé, afficher dans le drawer :
```
┌────────────────────────────────────────┐
│  📅 RENDEZ-VOUS                        │
│  ─────────────────────────────────     │
│  📍 Agence Paris 15ème                 │
│  🗓  Mercredi 26 février à 14h30       │
│  🎯 Service : Titre de séjour         │
│  ✅ Confirmé par SMS                   │
│                                        │
│  [Modifier]  [Annuler]  [Rappeler]     │
└────────────────────────────────────────┘
```

### Fichiers à modifier
- `components/sales/SalesDashboard.tsx` → Section drawer (refonte CTA dynamiques)

---

## 🔬 Phase 4 : Lien vers le Simulateur depuis la fiche

Quand le prospect est en "RDV Effectué", le commercial en agence doit pouvoir :
1. **Ouvrir le simulateur** pré-rempli avec les infos du prospect
2. **Voir le résultat** d'éligibilité
3. Si éligible → **Signer et ouvrir le dossier**

### Bouton "Ouvrir Simulateur"
- Redirige vers `/admin/simulator?prospectId=XXX&serviceId=YYY`
- Le simulateur se pré-remplit avec `prospect.firstName`, `prospect.phone`, `prospect.interestServiceId`
- Le résultat d'éligibilité est sauvegardé sur le prospect

### Ajouter au modèle `Prospect`
```typescript
// Dans services/SalesStore.ts → interface Prospect
eligibilityResult?: {
    isEligible: boolean;
    matchedProcedures: string[];    // IDs des procédures éligibles
    evaluatedAt: string;            // Date d'évaluation
};
```

### Fichiers à modifier
- **MODIFIER** `components/sales/SalesDashboard.tsx` → Ajouter bouton "Ouvrir Simulateur"
- **MODIFIER** `components/SimulatorWrapper.tsx` → Accepter un `prospectId` pour pré-remplissage
- **MODIFIER** `services/SalesStore.ts` → Ajouter `saveEligibilityResult()`

---

## ✍️ Phase 5 : Signature = Vraie conversion

La conversion (actuel `handleConvert`) ne doit se produire qu'**après le RDV en agence** + simulation validée.

### Nouveau workflow de signature
```
1. Commercial confirme "RDV Effectué"
2. Déroule le simulateur en agence → résultat éligible
3. Clique "Convertir en dossier" (disponible UNIQUEMENT si éligible)
4. → Crée le Lead dans le CRM avec :
   - Infos prospect (nom, tel, email)
   - Service validé (pas juste "intérêt", mais procédure exacte issue du simulateur)
   - Documents requis (basés sur la procédure éligible)
   - Agence d'origine
   - Référence du RDV
5. → Passe le prospect en "Signé"
6. → Le dossier apparaît dans /admin/leads (CRM Dossiers Clients)
```

### Garder `handleConvert` mais le conditionner
- Visible uniquement si `status === 'APPOINTMENT_DONE'`
- Bloqué si `eligibilityResult?.isEligible !== true`
- Message d'erreur si non éligible : "Le simulateur n'a pas trouvé de procédure éligible"

### Fichiers à modifier
- `components/sales/SalesDashboard.tsx` → Conditionner `handleConvert`

---

## 📱 Phase 6 : Confirmation RDV automatique

### Envoi automatique lors du passage en "RDV Fixé"
Quand le commercial fixe un RDV, envoyer automatiquement :
- **SMS** de confirmation avec date/heure/adresse
- **WhatsApp** avec les documents à apporter

### Template WhatsApp suggéré
```
Bonjour {firstName} 👋

Votre rendez-vous est confirmé :
📍 {agencyName} - {agencyAddress}
🗓 {appointmentDate} à {appointmentTime}
🎯 Service : {serviceName}

Documents à apporter :
✅ Pièce d'identité (passeport)
✅ Justificatif de domicile récent

À bientôt !
L'équipe SimuLegal
```

### Fichiers à modifier
- `services/SalesStore.ts` → `bookAppointment()` déclenche l'envoi
- `backend/src/whatsapp/whatsapp.service.ts` → Template de confirmation RDV

---

## 📌 Résumé des modifications par fichier

| Fichier | Modifications |
|---------|--------------|
| `services/SalesStore.ts` | Nouveaux statuts, champs RDV, `bookAppointment()`, `saveEligibilityResult()` |
| `components/sales/SalesDashboard.tsx` | Colonnes Kanban, CTA dynamiques, modal RDV, lien simulateur |
| **NOUVEAU** `components/sales/BookAppointmentModal.tsx` | Modal de prise de RDV avec date/agence |
| `components/SimulatorWrapper.tsx` | Accepter `prospectId` pour pré-remplissage |
| `backend/src/sales/sales.service.ts` | Nouveaux statuts, champs RDV en base |
| `backend/prisma/schema.prisma` | Champs RDV sur le modèle Prospect (si persisté) |

---

## 🎯 Ordre d'implémentation recommandé

1. **Phase 1** — Refonte colonnes Kanban (~15 min) → Impact visuel immédiat
2. **Phase 2** — Modal "Fixer un RDV" (~30 min) → Fonctionnalité clé
3. **Phase 3** — CTA dynamiques dans le drawer (~20 min) → UX cohérente
4. **Phase 4** — Lien simulateur depuis la fiche (~20 min) → Connexion avec le cœur métier
5. **Phase 5** — Conditionner la conversion (~15 min) → Logique métier correcte
6. **Phase 6** — Confirmation automatique (~15 min) → Finition / automatisation

**Temps total estimé : ~2h**
