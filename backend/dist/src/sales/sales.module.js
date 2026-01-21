"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SalesModule = void 0;
const common_1 = require("@nestjs/common");
const sales_service_1 = require("./sales.service");
const sales_controller_1 = require("./sales.controller");
const prisma_module_1 = require("../prisma/prisma.module");
const voice_module_1 = require("./voice/voice.module");
const call_logs_module_1 = require("./call-logs/call-logs.module");
const sales_analytics_service_1 = require("./sales-analytics.service");
const assignment_service_1 = require("./assignment.service");
let SalesModule = class SalesModule {
};
exports.SalesModule = SalesModule;
exports.SalesModule = SalesModule = __decorate([
    (0, common_1.Module)({
        imports: [prisma_module_1.PrismaModule, voice_module_1.VoiceModule, call_logs_module_1.CallLogsModule],
        providers: [sales_service_1.SalesService, sales_analytics_service_1.SalesAnalyticsService, assignment_service_1.AssignmentService],
        controllers: [sales_controller_1.SalesController],
        exports: [sales_service_1.SalesService, sales_analytics_service_1.SalesAnalyticsService, assignment_service_1.AssignmentService],
    })
], SalesModule);
//# sourceMappingURL=sales.module.js.map