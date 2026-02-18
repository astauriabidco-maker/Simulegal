"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var EligibilityService_1;
Object.defineProperty(exports, "__esModule", { value: true });
exports.EligibilityService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
let EligibilityService = EligibilityService_1 = class EligibilityService {
    prisma;
    logger = new common_1.Logger(EligibilityService_1.name);
    rootSpecsPath = path.join(process.cwd(), '..', 'specs');
    localSpecsPath = path.join(process.cwd(), 'specs');
    constructor(prisma) {
        this.prisma = prisma;
    }
    getSpecsDir() {
        if (fs.existsSync(this.rootSpecsPath))
            return this.rootSpecsPath;
        return this.localSpecsPath;
    }
    getFileNames() {
        return {
            'sejour': 'rules_sejour.json',
            'naturalisation': 'rules_naturalisation.json',
            'family': 'rules_family.json',
        };
    }
    getThresholds() {
        const specsDir = this.getSpecsDir();
        try {
            const filePath = path.join(specsDir, 'config_thresholds.json');
            if (fs.existsSync(filePath)) {
                return JSON.parse(fs.readFileSync(filePath, 'utf8'));
            }
        }
        catch (error) {
            this.logger.error('Failed to load thresholds', error);
        }
        throw new Error('Critical: Legal specification "config_thresholds.json" not found.');
    }
    getRules(category) {
        const specsDir = this.getSpecsDir();
        const fileName = this.getFileNames()[category];
        if (!fileName)
            return [];
        try {
            const filePath = path.join(specsDir, fileName);
            if (fs.existsSync(filePath)) {
                return JSON.parse(fs.readFileSync(filePath, 'utf8'));
            }
        }
        catch (error) {
            this.logger.error(`Failed to load rules for category ${category}`, error);
        }
        return [];
    }
    async evaluateEligibility(userProfile, category) {
        const rules = this.getRules(category);
        const thresholds = this.getThresholds();
        const { evaluateRule } = require('./rule-engine.util');
        const eligibleRules = rules.filter((rule) => evaluateRule(userProfile, rule.conditions, thresholds));
        return eligibleRules.sort((a, b) => (b.priority || 0) - (a.priority || 0));
    }
    async updateRule(category, ruleId, newConditions, changedBy, changeDetails) {
        const specsDir = this.getSpecsDir();
        const fileName = this.getFileNames()[category];
        if (!fileName)
            throw new Error(`Unknown category: ${category}`);
        const filePath = path.join(specsDir, fileName);
        const rules = JSON.parse(fs.readFileSync(filePath, 'utf8'));
        const ruleIndex = rules.findIndex((r) => r.id === ruleId);
        if (ruleIndex === -1)
            throw new Error(`Rule ${ruleId} not found in ${category}`);
        const previousRule = { ...rules[ruleIndex] };
        rules[ruleIndex].conditions = newConditions;
        fs.writeFileSync(filePath, JSON.stringify(rules, null, 4), 'utf8');
        await this.prisma.ruleAuditLog.create({
            data: {
                category,
                ruleId,
                ruleName: previousRule.title || previousRule.label || ruleId,
                action: 'UPDATE',
                changedBy,
                previousValue: JSON.stringify(previousRule.conditions),
                newValue: JSON.stringify(newConditions),
                changeDetails: changeDetails || `Conditions modifiées par ${changedBy}`,
            },
        });
        this.logger.log(`✅ Rule ${ruleId} updated in ${category} by ${changedBy}`);
        return rules[ruleIndex];
    }
    async updateThresholds(newThresholds, changedBy, changeDetails) {
        const specsDir = this.getSpecsDir();
        const filePath = path.join(specsDir, 'config_thresholds.json');
        let previousThresholds = {};
        try {
            previousThresholds = JSON.parse(fs.readFileSync(filePath, 'utf8'));
        }
        catch (e) { }
        fs.writeFileSync(filePath, JSON.stringify(newThresholds, null, 4), 'utf8');
        await this.prisma.ruleAuditLog.create({
            data: {
                category: 'config',
                ruleId: 'thresholds',
                ruleName: 'Seuils de configuration',
                action: 'UPDATE',
                changedBy,
                previousValue: JSON.stringify(previousThresholds),
                newValue: JSON.stringify(newThresholds),
                changeDetails: changeDetails || `Seuils mis à jour par ${changedBy}`,
            },
        });
        this.logger.log(`✅ Thresholds updated by ${changedBy}`);
        return newThresholds;
    }
    async getAuditLog(limit = 50) {
        return this.prisma.ruleAuditLog.findMany({
            orderBy: { createdAt: 'desc' },
            take: limit,
        });
    }
    async getRuleHistory(category, ruleId) {
        return this.prisma.ruleAuditLog.findMany({
            where: { category, ruleId },
            orderBy: { createdAt: 'desc' },
        });
    }
};
exports.EligibilityService = EligibilityService;
exports.EligibilityService = EligibilityService = EligibilityService_1 = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], EligibilityService);
//# sourceMappingURL=eligibility.service.js.map