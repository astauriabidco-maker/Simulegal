"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.CallLogsModule = void 0;
const common_1 = require("@nestjs/common");
const call_logs_controller_1 = require("./call-logs.controller");
const call_logs_service_1 = require("./call-logs.service");
const prisma_service_1 = require("../../prisma/prisma.service");
let CallLogsModule = class CallLogsModule {
};
exports.CallLogsModule = CallLogsModule;
exports.CallLogsModule = CallLogsModule = __decorate([
    (0, common_1.Module)({
        controllers: [call_logs_controller_1.CallLogsController],
        providers: [call_logs_service_1.CallLogsService, prisma_service_1.PrismaService],
        exports: [call_logs_service_1.CallLogsService],
    })
], CallLogsModule);
//# sourceMappingURL=call-logs.module.js.map