import { Injectable, CanActivate, ExecutionContext, Logger, ForbiddenException } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { validateRequest } from 'twilio';

/**
 * Guard NestJS qui valide la signature des webhooks entrants de Twilio.
 * 
 * Twilio signe chaque requête HTTP POST avec un header `X-Twilio-Signature`,
 * calculé à partir du Auth Token du compte, de l'URL complète et des paramètres POST.
 * 
 * Ce guard rejette toute requête dont la signature est invalide (anti-spoofing).
 * 
 * En développement (TWILIO_AUTH_TOKEN manquant), le guard laisse passer les requêtes
 * avec un warning, pour ne pas bloquer les tests locaux.
 */
@Injectable()
export class TwilioWebhookGuard implements CanActivate {
    private readonly logger = new Logger(TwilioWebhookGuard.name);
    private readonly authToken: string | undefined;
    private readonly webhookBaseUrl: string | undefined;

    constructor(private configService: ConfigService) {
        this.authToken = this.configService.get<string>('TWILIO_AUTH_TOKEN');
        this.webhookBaseUrl = this.configService.get<string>('TWILIO_WEBHOOK_URL');

        if (!this.authToken) {
            this.logger.warn('⚠️ TWILIO_AUTH_TOKEN non configuré. La validation des webhooks est DÉSACTIVÉE (mode dev).');
        } else {
            this.logger.log('🔒 Twilio webhook signature validation ENABLED');
        }
    }

    canActivate(context: ExecutionContext): boolean {
        // Si pas de token configuré, on laisse passer (mode développement)
        if (!this.authToken) {
            return true;
        }

        const request = context.switchToHttp().getRequest();

        // 1. Récupérer la signature Twilio depuis le header
        const twilioSignature = request.headers['x-twilio-signature'];

        if (!twilioSignature) {
            this.logger.warn('❌ Webhook rejeté: Header X-Twilio-Signature manquant');
            throw new ForbiddenException('Missing Twilio signature');
        }

        // 2. Reconstruire l'URL complète du webhook
        // En production derrière un reverse proxy, utiliser TWILIO_WEBHOOK_URL comme base
        let fullUrl: string;
        if (this.webhookBaseUrl) {
            // URL configurée explicitement (recommandé en production)
            fullUrl = this.webhookBaseUrl;
        } else {
            // Reconstruire depuis la requête (dev local)
            const protocol = request.headers['x-forwarded-proto'] || request.protocol || 'http';
            const host = request.headers['x-forwarded-host'] || request.headers['host'];
            fullUrl = `${protocol}://${host}${request.originalUrl}`;
        }

        // 3. Valider la signature avec le SDK Twilio
        const params = request.body || {};

        const isValid = validateRequest(
            this.authToken,
            twilioSignature,
            fullUrl,
            params
        );

        if (!isValid) {
            this.logger.warn(`❌ Webhook rejeté: Signature invalide pour ${fullUrl}`);
            this.logger.debug(`  Signature reçue: ${twilioSignature}`);
            this.logger.debug(`  Params: ${JSON.stringify(Object.keys(params))}`);
            throw new ForbiddenException('Invalid Twilio signature');
        }

        this.logger.log(`✅ Webhook validé: Signature Twilio authentique`);
        return true;
    }
}
