-- CreateTable
CREATE TABLE "BlogAutoTopic" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "title" TEXT NOT NULL,
    "summary" TEXT NOT NULL,
    "sourceUrl" TEXT,
    "sourceName" TEXT,
    "category" TEXT NOT NULL DEFAULT 'GENERAL',
    "relevanceScore" INTEGER NOT NULL DEFAULT 50,
    "keywords" TEXT,
    "status" TEXT NOT NULL DEFAULT 'DISCOVERED',
    "generatedArticleId" TEXT,
    "legalUpdateId" TEXT,
    "error" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL
);

-- CreateTable
CREATE TABLE "BlogAutoConfig" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "enabled" BOOLEAN NOT NULL DEFAULT true,
    "frequency" TEXT NOT NULL DEFAULT 'WEEKLY',
    "maxArticlesPerRun" INTEGER NOT NULL DEFAULT 2,
    "minRelevanceScore" INTEGER NOT NULL DEFAULT 60,
    "targetCategories" TEXT NOT NULL DEFAULT 'IMMIGRATION,NATURALISATION,SEJOUR,PERMIS,FAMILY',
    "aiModel" TEXT NOT NULL DEFAULT 'gpt-4o-mini',
    "aiPromptTemplate" TEXT,
    "lastRunAt" DATETIME,
    "totalGenerated" INTEGER NOT NULL DEFAULT 0,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL
);
