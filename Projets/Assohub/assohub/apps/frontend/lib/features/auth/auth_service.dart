import 'package:dio/dio.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:frontend/core/api_client.dart';

class AuthService {
  final ApiClient _apiClient = ApiClient();
  final _storage = const FlutterSecureStorage();

  Future<Map<String, dynamic>> register({
    required String associationName,
    required String adminEmail,
    required String adminPassword,
    String? firstName,
    String? lastName,
    String? phone,
  }) async {
    try {
      final response = await _apiClient.dio.post('/auth/register', data: {
        'associationName': associationName,
        'adminEmail': adminEmail,
        'adminPassword': adminPassword,
        'adminFirstName': firstName,
        'adminLastName': lastName,
        'phone': phone,
      });

      if (response.statusCode == 201) {
        final data = response.data;
        await _storage.write(key: 'access_token', value: data['access_token']);
        return data;
      }
      throw Exception('Erreur lors de l\'inscription');
    } on DioException catch (e) {
      throw Exception(e.response?.data['message'] ?? 'Erreur réseau');
    }
  }

  Future<Map<String, dynamic>> login(String email, String password) async {
    try {
      final response = await _apiClient.dio.post('/auth/login', data: {
        'email': email,
        'password': password,
      });

      if (response.statusCode == 201 || response.statusCode == 200) {
        final data = response.data;
        await _storage.write(key: 'access_token', value: data['access_token']);
        return data;
      }
      throw Exception('Erreur de connexion');
    } on DioException catch (e) {
      throw Exception(e.response?.data['message'] ?? 'Identifiants invalides');
    }
  }

  Future<void> logout() async {
    await _storage.delete(key: 'access_token');
  }

  Future<String?> getToken() async {
    return await _storage.read(key: 'access_token');
  }
}
