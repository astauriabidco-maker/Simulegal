-- CreateTable
CREATE TABLE "AiDashboardWidget" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "userId" TEXT NOT NULL,
    "agencyId" TEXT,
    "title" TEXT NOT NULL,
    "description" TEXT,
    "prompt" TEXT,
    "widgetType" TEXT NOT NULL,
    "querySql" TEXT NOT NULL,
    "mappingConfig" TEXT NOT NULL,
    "position" INTEGER NOT NULL DEFAULT 0,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL
);
