-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
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
    "documentCatalog" TEXT NOT NULL DEFAULT '{}',
    "serviceTemplates" TEXT NOT NULL DEFAULT '{}',
    "pipelineAutomations" TEXT NOT NULL DEFAULT '{}',
    "updatedAt" DATETIME NOT NULL
);
INSERT INTO "new_SystemSettings" ("catalogOverrides", "company", "id", "integrations", "legalDocuments", "notifications", "payment", "pipelineAutomations", "servicePricing", "servicePricingHistory", "storage", "updatedAt") SELECT "catalogOverrides", "company", "id", "integrations", "legalDocuments", "notifications", "payment", "pipelineAutomations", "servicePricing", "servicePricingHistory", "storage", "updatedAt" FROM "SystemSettings";
DROP TABLE "SystemSettings";
ALTER TABLE "new_SystemSettings" RENAME TO "SystemSettings";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
