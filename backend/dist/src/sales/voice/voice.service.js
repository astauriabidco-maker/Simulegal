"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.VoiceService = void 0;
const common_1 = require("@nestjs/common");
const config_1 = require("@nestjs/config");
const twilio_1 = __importDefault(require("twilio"));
const VoiceResponse = require('twilio').twiml.VoiceResponse;
let VoiceService = class VoiceService {
    configService;
    twilioClient;
    constructor(configService) {
        this.configService = configService;
        const accountSid = this.configService.get('TWILIO_ACCOUNT_SID');
        const authToken = this.configService.get('TWILIO_AUTH_TOKEN');
        if (accountSid && authToken) {
            this.twilioClient = (0, twilio_1.default)(accountSid, authToken);
        }
    }
    generateToken(identity) {
        const AccessToken = twilio_1.default.jwt.AccessToken;
        const VoiceGrant = AccessToken.VoiceGrant;
        const twilioAccountSid = this.configService.get('TWILIO_ACCOUNT_SID');
        const twilioApiKey = this.configService.get('TWILIO_API_KEY');
        const twilioApiSecret = this.configService.get('TWILIO_API_SECRET');
        const outgoingApplicationSid = this.configService.get('TWILIO_APP_SID');
        if (!twilioAccountSid || !twilioApiKey || !twilioApiSecret || !outgoingApplicationSid) {
            throw new Error('Twilio credentials missing in configuration');
        }
        const token = new AccessToken(twilioAccountSid, twilioApiKey, twilioApiSecret, { identity: identity });
        const voiceGrant = new VoiceGrant({
            outgoingApplicationSid: outgoingApplicationSid,
            incomingAllow: true,
        });
        token.addGrant(voiceGrant);
        return {
            token: token.toJwt(),
            identity: identity
        };
    }
    handleIncomingCall(from, to) {
        const response = new VoiceResponse();
        const dial = response.dial({ callerId: to });
        dial.client('support_agent');
        return response.toString();
    }
    handleOutgoingCall(to) {
        const response = new VoiceResponse();
        const callerId = this.configService.get('TWILIO_CALLER_ID');
        const dial = response.dial({ callerId: callerId });
        dial.number(to);
        return response.toString();
    }
};
exports.VoiceService = VoiceService;
exports.VoiceService = VoiceService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [config_1.ConfigService])
], VoiceService);
//# sourceMappingURL=voice.service.js.map