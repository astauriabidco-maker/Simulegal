import 'package:flutter/material.dart';
import 'package:frontend/features/auth/auth_service.dart';

class AuthProvider extends ChangeNotifier {
  final AuthService _authService = AuthService();
  bool _isAuthenticated = false;
  bool _isLoading = false;
  String? _error;

  bool get isAuthenticated => _isAuthenticated;
  bool get isLoading => _isLoading;
  String? get error => _error;

  AuthProvider() {
    _checkAuth();
  }

  Future<void> _checkAuth() async {
    final token = await _authService.getToken();
    _isAuthenticated = token != null;
    notifyListeners();
  }

  Future<bool> register({
    required String associationName,
    required String adminEmail,
    required String adminPassword,
    String? firstName,
    String? lastName,
    String? phone,
  }) async {
    _setLoading(true);
    _error = null;
    try {
      await _authService.register(
        associationName: associationName,
        adminEmail: adminEmail,
        adminPassword: adminPassword,
        firstName: firstName,
        lastName: lastName,
        phone: phone,
      );
      _isAuthenticated = true;
      _setLoading(false);
      return true;
    } catch (e) {
      _error = e.toString();
      _setLoading(false);
      return false;
    }
  }

  Future<bool> login(String email, String password) async {
    _setLoading(true);
    _error = null;
    try {
      await _authService.login(email, password);
      _isAuthenticated = true;
      _setLoading(false);
      return true;
    } catch (e) {
      _error = e.toString();
      _setLoading(false);
      return false;
    }
  }

  Future<void> logout() async {
    await _authService.logout();
    _isAuthenticated = false;
    notifyListeners();
  }

  void _setLoading(bool value) {
    _isLoading = value;
    notifyListeners();
  }
}
