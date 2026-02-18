import { Controller, Get, Post, Put, Param, Body, Query } from '@nestjs/common';
import { EligibilityService } from './eligibility.service';

@Controller('eligibility')
export class EligibilityController {
    constructor(private readonly service: EligibilityService) { }

    @Get('thresholds')
    getThresholds() {
        return this.service.getThresholds();
    }

    @Get('rules/:category')
    getRules(@Param('category') category: string) {
        return this.service.getRules(category);
    }

    @Post('evaluate/:category')
    evaluate(@Param('category') category: string, @Body() userProfile: any) {
        return this.service.evaluateEligibility(userProfile, category);
    }

    // ── Audit Trail endpoints ─────────────────────────────────

    @Put('rules/:category/:ruleId')
    updateRule(
        @Param('category') category: string,
        @Param('ruleId') ruleId: string,
        @Body() body: { conditions: any; changedBy: string; changeDetails?: string },
    ) {
        return this.service.updateRule(category, ruleId, body.conditions, body.changedBy, body.changeDetails);
    }

    @Put('thresholds')
    updateThresholds(
        @Body() body: { thresholds: any; changedBy: string; changeDetails?: string },
    ) {
        return this.service.updateThresholds(body.thresholds, body.changedBy, body.changeDetails);
    }

    @Get('audit-log')
    getAuditLog(@Query('limit') limit?: string) {
        return this.service.getAuditLog(limit ? parseInt(limit) : 50);
    }

    @Get('audit-log/:category/:ruleId')
    getRuleHistory(
        @Param('category') category: string,
        @Param('ruleId') ruleId: string,
    ) {
        return this.service.getRuleHistory(category, ruleId);
    }
}
