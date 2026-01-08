"""
Script utilitaire pour tester la configuration WhatsApp (Twilio).
Usage: python scripts/test_whatsapp.py <numero_destinataire>
Exemple: python scripts/test_whatsapp.py +33612345678
"""
import sys
import os
import asyncio
from dotenv import load_dotenv

# Ajouter le dossier racine au path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from core.notifications import NotificationService

# Charger les variables d'env
load_dotenv()

async def main():
    if len(sys.argv) < 2:
        print("Usage: python scripts/test_whatsapp.py <numero_destinataire>")
        print("Note: Le numéro doit être au format international (ex: +336...)")
        return

    to_number = sys.argv[1]
    service = NotificationService()
    
    print("\n📱 Test d'envoi WhatsApp SimuLegal")
    print("-----------------------------------")
    print(f"Vers: {to_number}")
    print(f"SID configuré: {'✅ Oui' if service.twilio_sid else '❌ Non'}")
    print(f"Token configuré: {'✅ Oui' if service.twilio_token else '❌ Non'}")
    print(f"Numéro source: {service.twilio_phone or '❌ Non configuré'}")
    
    if not service.twilio_sid:
        print("\n❌ Erreur: TWILIO_SID manquant dans le .env")
        return

    print("\nEnvoi en cours...")
    success = await service.send_whatsapp(
        to=to_number,
        message="🔔 Ceci est un test de notification WhatsApp SimuLegal.\n\nSi vous lisez ce message, l'intégration Twilio fonctionne ! ✅"
    )
    
    if success:
        print("\n✅ Message envoyé avec succès !")
    else:
        print("\n❌ Échec de l'envoi. Vérifiez les logs ci-dessus.")

if __name__ == "__main__":
    asyncio.run(main())
