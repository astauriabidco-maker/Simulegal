import { Injectable } from '@nestjs/common';

@Injectable()
export class CatalogService {
    // In real app, fetch from DB
    getServices() {
        return [
            {
                id: 'naturalisation',
                name: '🇫🇷 Naturalisation (Base)',
                description: 'Demande de nationalité française - cas général',
                category: 'IMMIGRATION'
            },
            // ... (Other services would come from DB)
        ];
    }
}
