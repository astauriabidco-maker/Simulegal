import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';
import 'package:frontend/features/auth/auth_provider.dart';
import 'package:frontend/features/auth/login_screen.dart';
import 'package:frontend/features/auth/register_screen.dart';
import 'package:frontend/features/dashboard/dashboard_screen.dart';
import 'package:frontend/features/members/screens/members_list_screen.dart';

class AppRouter {
  static GoRouter router(BuildContext context) {
    return GoRouter(
      initialLocation: '/login',
      refreshListenable: context.read<AuthProvider>(),
      redirect: (context, state) {
        final authProvider = context.read<AuthProvider>();
        final isLoggingIn = state.matchedLocation == '/login' || state.matchedLocation == '/register';

        if (!authProvider.isAuthenticated && !isLoggingIn) {
          return '/login';
        }

        if (authProvider.isAuthenticated && isLoggingIn) {
          return '/dashboard';
        }

        return null;
      },
      routes: [
        GoRoute(
          path: '/login',
          builder: (context, state) => const LoginScreen(),
        ),
        GoRoute(
          path: '/register',
          builder: (context, state) => const RegisterScreen(),
        ),
        GoRoute(
          path: '/dashboard',
          builder: (context, state) => const DashboardScreen(),
        ),
        GoRoute(
          path: '/members',
          builder: (context, state) => const DashboardScreen(initialIndex: 1),
        ),
      ],
    );
  }
}
