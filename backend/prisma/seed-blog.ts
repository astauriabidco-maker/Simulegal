/**
 * Seed Blog Articles — Run with: npx tsx prisma/seed-blog.ts
 */
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

const ARTICLES = [
    {
        title: 'Naturalisation française : le guide complet 2026',
        slug: 'naturalisation-francaise-guide-complet-2026',
        excerpt: 'Tout ce qu\'il faut savoir pour préparer et réussir votre demande de naturalisation par décret ou par mariage en 2026.',
        content: `## Qu'est-ce que la naturalisation française ?

La naturalisation est le processus par lequel un ressortissant étranger acquiert la nationalité française. En 2026, les critères restent stricts mais accessibles avec une bonne préparation.

### Les deux voies principales

**1. Naturalisation par décret**
- Résidence continue en France depuis **5 ans minimum**
- Niveau de français **B2** (oral et écrit)
- Ressources stables et suffisantes
- Casier judiciaire vierge
- Adhésion aux valeurs de la République

**2. Naturalisation par mariage**
- Marié(e) avec un(e) ressortissant(e) français(e) depuis **4 ans**
- Communauté de vie effective
- Niveau de français **B1** (si résidence en France)

### Documents essentiels à préparer

1. Justificatif d'identité et de nationalité
2. Justificatif de domicile (3 derniers mois)
3. Avis d'imposition des 3 dernières années
4. Diplôme de français ou attestation de niveau
5. Certificat de casier judiciaire du pays d'origine

### Délais moyens

Le délai de traitement est en moyenne de **12 à 18 mois** après le dépôt complet du dossier.

> **Conseil SimuLegal** : Utilisez notre simulateur d'éligibilité pour vérifier vos chances avant de constituer votre dossier.`,
        category: 'NATURALISATION',
        status: 'PUBLISHED',
        authorName: 'Maître A. Dupont',
        authorRole: 'Juriste en droit des étrangers',
        tags: 'naturalisation,nationalité française,décret,mariage',
        featured: true,
        readTimeMin: 8,
        publishedAt: new Date('2026-02-15'),
    },
    {
        title: 'Titre de séjour VPF : conditions et démarches',
        slug: 'titre-sejour-vpf-conditions-demarches',
        excerpt: 'Comprendre les conditions d\'obtention du titre de séjour « Vie Privée et Familiale » pour les conjoints de Français et parents d\'enfants français.',
        content: `## Le titre de séjour VPF en détail

Le titre de séjour "Vie Privée et Familiale" (VPF) est l'un des titres les plus demandés en France. Il permet de résider et travailler en France.

### Pour qui ?

- **Conjoints de Français** : marié(e) avec un(e) Français(e)
- **Parents d'enfant français** : ayant un enfant né en France de nationalité française
- **Liens personnels et familiaux** : justifiant de fortes attaches en France

### Conditions pour le conjoint de Français

- Mariage célébré et transcrit
- Communauté de vie effective
- Entrée régulière en France
- Niveau de français **A2** minimum
- Examen civique réussi

### Renouvellement

Le titre VPF est délivré pour **1 an** la première fois, renouvelable. Après **3 ans**, vous pouvez demander une carte de résident (10 ans).

### Pièces à fournir

1. Acte de mariage transcrit
2. Justificatif de communauté de vie
3. Justificatif de domicile commun
4. Attestation de niveau de français A2
5. Visa long séjour ou récépissé

> **Astuce** : Consultez notre simulateur en ligne pour connaître immédiatement votre éligibilité au titre VPF.`,
        category: 'SEJOUR',
        status: 'PUBLISHED',
        authorName: 'Rédaction SimuLegal',
        authorRole: 'Équipe juridique',
        tags: 'titre de séjour,VPF,conjoint français,parent enfant français',
        featured: true,
        readTimeMin: 6,
        publishedAt: new Date('2026-02-10'),
    },
    {
        title: 'Échange de permis de conduire étranger : nouvelles règles 2026',
        slug: 'echange-permis-conduire-etranger-2026',
        excerpt: 'Les nouvelles modalités d\'échange de permis de conduire étranger en permis français. Zones concernées, délais et procédure.',
        content: `## Échange de permis étranger en France

Depuis les réformes récentes, l'échange d'un permis de conduire étranger suit des règles spécifiques selon votre pays d'origine.

### Pays concernés par l'échange automatique

- **Union Européenne / EEE** : reconnaissance mutuelle
- **Pays avec accord bilatéral** : Algérie, Maroc, Tunisie, Turquie, etc.
- **Autres pays** : examen de conduite simplifié possible

### Conditions générales

1. Permis valide et en cours de validité
2. Permis obtenu **avant** l'installation en France
3. Résidence normale en France depuis au moins **6 mois**
4. Pas de restriction de conduite dans le pays d'origine

### Délais

- Dépôt en ligne sur le site de l'ANTS
- Délai moyen : **3 à 6 mois**
- Pendant l'instruction, vous pouvez conduire avec votre permis étranger (1 an max)

> **Important** : Certains pays ne sont pas éligibles à l'échange. Dans ce cas, il faudra repasser le permis français.`,
        category: 'PERMIS',
        status: 'PUBLISHED',
        authorName: 'Rédaction SimuLegal',
        tags: 'permis de conduire,échange,ANTS,conduite',
        featured: false,
        readTimeMin: 5,
        publishedAt: new Date('2026-02-05'),
    },
    {
        title: 'Regroupement familial : étapes clés et conditions de ressources',
        slug: 'regroupement-familial-etapes-conditions',
        excerpt: 'Votre guide pratique pour comprendre les étapes du regroupement familial et les conditions de revenus exigées.',
        content: `## Regroupement familial en France

Le regroupement familial permet à un ressortissant étranger résidant régulièrement en France de faire venir sa famille.

### Conditions principales

- Résidence en France depuis **18 mois minimum**
- Titre de séjour valide d'au moins 1 an
- Ressources stables (au moins le SMIC)
- Logement adapté à la taille de la famille

### Procédure

1. Dépôt du dossier auprès de l'OFII
2. Vérification du logement par la mairie
3. Vérification des ressources
4. Décision du préfet (6 mois max)
5. Demande de visa par la famille

### Cas particulier : accord franco-algérien

Les ressortissants algériens bénéficient de dispositions spécifiques avec un délai de résidence réduit.

> **SimuLegal vous aide** : Testez gratuitement votre éligibilité au regroupement familial avec notre simulateur intelligent.`,
        category: 'FAMILY',
        status: 'DRAFT',
        authorName: 'Rédaction SimuLegal',
        tags: 'regroupement familial,OFII,famille,logement',
        featured: false,
        readTimeMin: 5,
    },
];

async function seed() {
    console.log('🌱 Seeding blog articles...');

    for (const article of ARTICLES) {
        const existing = await prisma.article.findUnique({ where: { slug: article.slug } });
        if (existing) {
            console.log(`  ⏭️  Skip (exists): ${article.title}`);
            continue;
        }
        await prisma.article.create({ data: article });
        console.log(`  ✅ Created: ${article.title}`);
    }

    const count = await prisma.article.count();
    console.log(`\n📚 Total articles in DB: ${count}`);
}

seed()
    .catch(console.error)
    .finally(() => prisma.$disconnect());
