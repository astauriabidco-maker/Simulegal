import 'package:dio/dio.dart';
import 'package:frontend/core/api_client.dart';
import '../models/member.dart';

class MembersService {
  final ApiClient _apiClient = ApiClient();

  Future<List<Member>> getMembers() async {
    try {
      final response = await _apiClient.dio.get('/users');
      final List<dynamic> data = response.data;
      return data.map((json) => Member.fromJson(json)).toList();
    } catch (e) {
      rethrow;
    }
  }

  Future<Member> createMember({
    required String email,
    String? firstName,
    String? lastName,
    String role = 'MEMBER',
  }) async {
    try {
      final response = await _apiClient.dio.post('/users', data: {
        'email': email,
        'firstName': firstName,
        'lastName': lastName,
        'role': role,
      });
      return Member.fromJson(response.data);
    } catch (e) {
      rethrow;
    }
  }

  Future<Member> updateMember(String id, Map<String, dynamic> data) async {
    try {
      final response = await _apiClient.dio.patch('/users/$id', data: data);
      return Member.fromJson(response.data);
    } catch (e) {
      rethrow;
    }
  }

  Future<void> deleteMember(String id) async {
    try {
      await _apiClient.dio.delete('/users/$id');
    } catch (e) {
      rethrow;
    }
  }
}
