import { Controller, Get, Post, Put, Delete, Body, Param, Query } from '@nestjs/common';
import { PipelineAutomationService, AutomationRule } from './pipeline-automation.service';

@Controller('pipeline-automations')
export class PipelineAutomationController {
    constructor(private readonly automationService: PipelineAutomationService) { }

    // ═══════════════════════════════════════════════
    // CRUD DES RÈGLES
    // ═══════════════════════════════════════════════

    @Get('rules')
    async getRules() {
        const rules = await this.automationService.getRules();
        return { rules, count: rules.length };
    }

    @Get('rules/:id')
    async getRule(@Param('id') id: string) {
        const rule = await this.automationService.getRule(id);
        if (!rule) return { error: 'Rule not found' };
        return rule;
    }

    @Post('rules')
    async createRule(@Body() body: Partial<AutomationRule>) {
        const rule = await this.automationService.createRule(body);
        return { success: true, rule };
    }

    @Put('rules/:id')
    async updateRule(@Param('id') id: string, @Body() body: Partial<AutomationRule>) {
        const rule = await this.automationService.updateRule(id, body);
        if (!rule) return { error: 'Rule not found' };
        return { success: true, rule };
    }

    @Delete('rules/:id')
    async deleteRule(@Param('id') id: string) {
        const success = await this.automationService.deleteRule(id);
        return { success };
    }

    @Post('rules/:id/toggle')
    async toggleRule(@Param('id') id: string) {
        const rule = await this.automationService.toggleRule(id);
        if (!rule) return { error: 'Rule not found' };
        return { success: true, rule };
    }

    // ═══════════════════════════════════════════════
    // LOGS & STATS
    // ═══════════════════════════════════════════════

    @Get('logs')
    async getLogs(@Query('limit') limit?: string) {
        const logs = await this.automationService.getLogs(limit ? parseInt(limit) : 100);
        return { logs, count: logs.length };
    }

    @Get('stats')
    async getStats() {
        return this.automationService.getStats();
    }

    // ═══════════════════════════════════════════════
    // ACTIONS MANUELLES
    // ═══════════════════════════════════════════════

    @Post('check-stale')
    async checkStaleNow() {
        const result = await this.automationService.checkStaleLeads();
        return { success: true, ...result };
    }

    @Post('trigger-stage-change')
    async triggerStageChange(@Body() data: { lead: any; oldStage: string; newStage: string }) {
        await this.automationService.onStageChange(data.lead, data.oldStage, data.newStage);
        return { success: true };
    }

    @Post('trigger-docs-complete')
    async triggerDocsComplete(@Body() data: { lead: any }) {
        await this.automationService.onDocsComplete(data.lead);
        return { success: true };
    }

    @Post('trigger-payment')
    async triggerPayment(@Body() data: { lead: any }) {
        await this.automationService.onPaymentReceived(data.lead);
        return { success: true };
    }
}
