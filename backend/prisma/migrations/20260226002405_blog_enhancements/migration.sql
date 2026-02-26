-- AlterTable
ALTER TABLE "Article" ADD COLUMN "scheduledAt" DATETIME;

-- AlterTable
ALTER TABLE "Prospect" ADD COLUMN "tags" TEXT;

-- CreateTable
CREATE TABLE "ArticleComment" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "articleId" TEXT NOT NULL,
    "authorName" TEXT NOT NULL,
    "authorEmail" TEXT,
    "content" TEXT NOT NULL,
    "status" TEXT NOT NULL DEFAULT 'PENDING',
    "parentId" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT "ArticleComment_articleId_fkey" FOREIGN KEY ("articleId") REFERENCES "Article" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "ArticleComment_parentId_fkey" FOREIGN KEY ("parentId") REFERENCES "ArticleComment" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ArticleRevision" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "articleId" TEXT NOT NULL,
    "title" TEXT NOT NULL,
    "content" TEXT NOT NULL,
    "excerpt" TEXT,
    "editedBy" TEXT,
    "version" INTEGER NOT NULL DEFAULT 1,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT "ArticleRevision_articleId_fkey" FOREIGN KEY ("articleId") REFERENCES "Article" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ArticleReaction" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "articleId" TEXT NOT NULL,
    "type" TEXT NOT NULL,
    "ipHash" TEXT NOT NULL,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT "ArticleReaction_articleId_fkey" FOREIGN KEY ("articleId") REFERENCES "Article" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ArticleAnalytics" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "articleId" TEXT NOT NULL,
    "date" DATETIME NOT NULL,
    "views" INTEGER NOT NULL DEFAULT 0,
    "uniqueIps" INTEGER NOT NULL DEFAULT 0,
    "avgTimeMs" INTEGER,
    "referrer" TEXT,
    CONSTRAINT "ArticleAnalytics_articleId_fkey" FOREIGN KEY ("articleId") REFERENCES "Article" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "NewsletterSubscriber" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "email" TEXT NOT NULL,
    "name" TEXT,
    "status" TEXT NOT NULL DEFAULT 'ACTIVE',
    "subscribedAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "unsubscribedAt" DATETIME,
    "confirmToken" TEXT,
    "confirmed" BOOLEAN NOT NULL DEFAULT false
);

-- CreateTable
CREATE TABLE "BlogCategory" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "slug" TEXT NOT NULL,
    "icon" TEXT,
    "color" TEXT,
    "sortOrder" INTEGER NOT NULL DEFAULT 0,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- CreateTable
CREATE TABLE "SalesObjective" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "userId" TEXT NOT NULL,
    "agencyId" TEXT,
    "period" TEXT NOT NULL,
    "periodType" TEXT NOT NULL DEFAULT 'MONTHLY',
    "metric" TEXT NOT NULL,
    "targetValue" INTEGER NOT NULL,
    "currentValue" INTEGER NOT NULL DEFAULT 0,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL
);

-- CreateTable
CREATE TABLE "SalesActivity" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "userId" TEXT NOT NULL,
    "agencyId" TEXT,
    "date" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "activityType" TEXT NOT NULL,
    "prospectId" TEXT,
    "prospectName" TEXT,
    "outcome" TEXT,
    "duration" INTEGER,
    "notes" TEXT,
    "metadata" JSONB,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- CreateTable
CREATE TABLE "SalesCommission" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "userId" TEXT NOT NULL,
    "agencyId" TEXT,
    "prospectId" TEXT,
    "prospectName" TEXT,
    "serviceId" TEXT,
    "serviceName" TEXT,
    "amountEuros" REAL NOT NULL,
    "rate" REAL NOT NULL,
    "baseAmount" REAL NOT NULL,
    "status" TEXT NOT NULL DEFAULT 'PENDING',
    "paidAt" DATETIME,
    "period" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL
);

-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_Lead" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "email" TEXT NOT NULL,
    "phone" TEXT NOT NULL,
    "serviceId" TEXT NOT NULL,
    "serviceName" TEXT NOT NULL,
    "amountPaid" INTEGER NOT NULL DEFAULT 0,
    "paymentMethod" TEXT DEFAULT 'UNPAID',
    "paymentDate" DATETIME,
    "paymentRef" TEXT,
    "invoiceNumber" TEXT,
    "status" TEXT NOT NULL DEFAULT 'NEW',
    "originAgencyId" TEXT,
    "contract" TEXT,
    "documents" TEXT NOT NULL DEFAULT '[]',
    "requiredDocs" TEXT,
    "assignedUserId" TEXT,
    "stageEnteredAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "stageHistory" TEXT NOT NULL DEFAULT '[]',
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    "data" TEXT NOT NULL DEFAULT '{}',
    CONSTRAINT "Lead_assignedUserId_fkey" FOREIGN KEY ("assignedUserId") REFERENCES "User" ("id") ON DELETE SET NULL ON UPDATE CASCADE,
    CONSTRAINT "Lead_originAgencyId_fkey" FOREIGN KEY ("originAgencyId") REFERENCES "Agency" ("id") ON DELETE SET NULL ON UPDATE CASCADE
);
INSERT INTO "new_Lead" ("amountPaid", "assignedUserId", "contract", "createdAt", "data", "documents", "email", "id", "invoiceNumber", "name", "originAgencyId", "paymentDate", "paymentMethod", "paymentRef", "phone", "requiredDocs", "serviceId", "serviceName", "status", "updatedAt") SELECT "amountPaid", "assignedUserId", "contract", "createdAt", "data", "documents", "email", "id", "invoiceNumber", "name", "originAgencyId", "paymentDate", "paymentMethod", "paymentRef", "phone", "requiredDocs", "serviceId", "serviceName", "status", "updatedAt" FROM "Lead";
DROP TABLE "Lead";
ALTER TABLE "new_Lead" RENAME TO "Lead";
CREATE TABLE "new_SystemSettings" (
    "id" TEXT NOT NULL PRIMARY KEY DEFAULT 'GLOBAL',
    "company" TEXT NOT NULL DEFAULT '{}',
    "payment" TEXT NOT NULL DEFAULT '{}',
    "notifications" TEXT NOT NULL DEFAULT '{}',
    "integrations" TEXT NOT NULL DEFAULT '{}',
    "storage" TEXT NOT NULL DEFAULT '{}',
    "servicePricing" TEXT NOT NULL DEFAULT '{}',
    "servicePricingHistory" TEXT NOT NULL DEFAULT '[]',
    "legalDocuments" TEXT NOT NULL DEFAULT '{}',
    "catalogOverrides" TEXT NOT NULL DEFAULT '{}',
    "pipelineAutomations" TEXT NOT NULL DEFAULT '{}',
    "updatedAt" DATETIME NOT NULL
);
INSERT INTO "new_SystemSettings" ("catalogOverrides", "company", "id", "integrations", "legalDocuments", "notifications", "payment", "pipelineAutomations", "servicePricing", "storage", "updatedAt") SELECT "catalogOverrides", "company", "id", "integrations", "legalDocuments", "notifications", "payment", "pipelineAutomations", "servicePricing", "storage", "updatedAt" FROM "SystemSettings";
DROP TABLE "SystemSettings";
ALTER TABLE "new_SystemSettings" RENAME TO "SystemSettings";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;

-- CreateIndex
CREATE UNIQUE INDEX "ArticleReaction_articleId_ipHash_type_key" ON "ArticleReaction"("articleId", "ipHash", "type");

-- CreateIndex
CREATE UNIQUE INDEX "ArticleAnalytics_articleId_date_key" ON "ArticleAnalytics"("articleId", "date");

-- CreateIndex
CREATE UNIQUE INDEX "NewsletterSubscriber_email_key" ON "NewsletterSubscriber"("email");

-- CreateIndex
CREATE UNIQUE INDEX "NewsletterSubscriber_confirmToken_key" ON "NewsletterSubscriber"("confirmToken");

-- CreateIndex
CREATE UNIQUE INDEX "BlogCategory_name_key" ON "BlogCategory"("name");

-- CreateIndex
CREATE UNIQUE INDEX "BlogCategory_slug_key" ON "BlogCategory"("slug");

-- CreateIndex
CREATE UNIQUE INDEX "SalesObjective_userId_period_metric_key" ON "SalesObjective"("userId", "period", "metric");
