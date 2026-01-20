import { Injectable } from '@nestjs/common';
import * as crypto from 'crypto';

@Injectable()
export class MeetingsService {
    /**
     * Generates a unique meeting link.
     * In a real scenario, this would call Google Calendar / Meet API via OAuth.
     * For now, we simulate by generating a unique but realistic Meet format slug.
     */
    async generateMeetingLink(): Promise<string> {
        // Generate a slug like abc-defg-hij
        const part1 = crypto.randomBytes(2).toString('hex').substring(0, 3);
        const part2 = crypto.randomBytes(2).toString('hex').substring(0, 4);
        const part3 = crypto.randomBytes(2).toString('hex').substring(0, 3);

        const slug = `${part1}-${part2}-${part3}`;
        return `https://meet.google.com/${slug}`;
    }
}
