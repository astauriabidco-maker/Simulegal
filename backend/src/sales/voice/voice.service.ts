import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import Twilio from 'twilio';
const VoiceResponse = require('twilio').twiml.VoiceResponse;

@Injectable()
export class VoiceService {
    private twilioClient: Twilio.Twilio;

    constructor(private configService: ConfigService) {
        const accountSid = this.configService.get<string>('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get<string>('TWILIO_AUTH_TOKEN');

        if (accountSid && authToken) {
            this.twilioClient = Twilio(accountSid, authToken);
        }
    }

    generateToken(identity: string) {
        const AccessToken = Twilio.jwt.AccessToken;
        const VoiceGrant = AccessToken.VoiceGrant;

        const twilioAccountSid = this.configService.get<string>('TWILIO_ACCOUNT_SID');
        const twilioApiKey = this.configService.get<string>('TWILIO_API_KEY');
        const twilioApiSecret = this.configService.get<string>('TWILIO_API_SECRET');
        const outgoingApplicationSid = this.configService.get<string>('TWILIO_APP_SID');

        if (!twilioAccountSid || !twilioApiKey || !twilioApiSecret || !outgoingApplicationSid) {
            throw new Error('Twilio credentials missing in configuration');
        }

        const token = new AccessToken(
            twilioAccountSid,
            twilioApiKey,
            twilioApiSecret,
            { identity: identity }
        );

        const voiceGrant = new VoiceGrant({
            outgoingApplicationSid: outgoingApplicationSid,
            incomingAllow: true, // Allow incoming calls to this identity
        });

        token.addGrant(voiceGrant);

        return {
            token: token.toJwt(),
            identity: identity
        };
    }

    handleIncomingCall(from: string, to: string) {
        const response = new VoiceResponse();
        const dial = response.dial({ callerId: to });
        dial.client('support_agent'); // Route to a specific client or logic
        return response.toString();
    }

    handleOutgoingCall(to: string) {
        const response = new VoiceResponse();
        const callerId = this.configService.get<string>('TWILIO_CALLER_ID'); // Verify this number in Twilio

        const dial = response.dial({ callerId: callerId });
        // If 'to' is a client name, dial client. If number, dial number.
        // For simplicity, we assume 'to' is a number.
        dial.number(to);

        return response.toString();
    }
}
