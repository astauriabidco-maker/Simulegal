import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:frontend/core/theme.dart';
import 'package:frontend/core/router.dart';
import 'package:frontend/features/auth/auth_provider.dart';

void main() {
  runApp(
    MultiProvider(
      providers: [
        ChangeNotifierProvider(create: (_) => AuthProvider()),
      ],
      child: const AssoHubApp(),
    ),
  );
}

class AssoHubApp extends StatelessWidget {
  const AssoHubApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp.router(
      title: 'ASSOSHUB',
      theme: AppTheme.lightTheme,
      routerConfig: AppRouter.router(context),
      debugShowCheckedModeBanner: false,
    );
  }
}
