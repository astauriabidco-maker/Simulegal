import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:frontend/features/auth/auth_provider.dart';
import 'package:frontend/features/members/screens/members_list_screen.dart';
import 'package:go_router/go_router.dart';

class DashboardScreen extends StatefulWidget {
  final int initialIndex;
  const DashboardScreen({super.key, this.initialIndex = 0});

  @override
  State<DashboardScreen> createState() => _DashboardScreenState();
}

class _DashboardScreenState extends State<DashboardScreen> {
  late int _currentIndex;
  final GlobalKey<MembersListScreenState> _membersListKey = GlobalKey<MembersListScreenState>();

  @override
  void initState() {
    super.initState();
    _currentIndex = widget.initialIndex;
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Row(
        children: [
          // Sidebar
          Container(
            width: 250,
            color: const Color(0xFF1A237E),
            child: Column(
              children: [
                const DrawerHeader(
                  child: Center(
                    child: Text(
                      'ASSOSHUB',
                      style: TextStyle(color: Colors.white, fontSize: 24, fontWeight: FontWeight.bold),
                    ),
                  ),
                ),
                Expanded(
                  child: SingleChildScrollView(
                    child: Column(
                      children: [
                        _SidebarItem(
                          key: const ValueKey('sidebar_ov'),
                          icon: Icons.dashboard,
                          label: "Vue d'ensemble",
                          isActive: _currentIndex == 0,
                          onTap: () => setState(() => _currentIndex = 0),
                        ),
                        _SidebarItem(
                          key: const ValueKey('sidebar_members'),
                          icon: Icons.people,
                          label: 'Membres',
                          isActive: _currentIndex == 1,
                          onTap: () => setState(() => _currentIndex = 1),
                        ),
                        _SidebarItem(
                          icon: Icons.account_balance_wallet,
                          label: 'Finances',
                          isActive: false,
                          onTap: () {},
                        ),
                        _SidebarItem(
                          icon: Icons.settings,
                          label: 'Paramètres',
                          isActive: false,
                          onTap: () {},
                        ),
                      ],
                    ),
                  ),
                ),
                _SidebarItem(
                  icon: Icons.logout,
                  label: 'Déconnexion',
                  isActive: false,
                  onTap: () {
                    context.read<AuthProvider>().logout();
                    context.go('/login');
                  },
                ),
                const SizedBox(height: 16),
              ],
            ),
          ),
          // Content
          Expanded(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                AppBar(
                  title: Text(_currentIndex == 0 ? 'Tableau de Bord' : 'Membres'),
                  elevation: 0,
                  backgroundColor: Colors.transparent,
                  foregroundColor: Colors.black,
                ),
                Expanded(
                  child: IndexedStack(
                    index: _currentIndex,
                    children: [
                      // Overview
                      const Padding(
                        padding: EdgeInsets.all(32),
                        child: Column(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            Text(
                              'Bienvenue sur votre espace ASSOSHUB',
                              style: TextStyle(fontSize: 28, fontWeight: FontWeight.bold),
                            ),
                            SizedBox(height: 8),
                            Text(
                              'Gérez votre association en toute simplicité.',
                              style: TextStyle(fontSize: 16, color: Colors.grey),
                            ),
                          ],
                        ),
                      ),
                      // Members
                      MembersListScreen(key: _membersListKey),
                    ],
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
      floatingActionButton: _currentIndex == 1
          ? FloatingActionButton(
              onPressed: () {
                _membersListKey.currentState?.showAddMemberDialog(context);
              },
              child: const Icon(Icons.add),
            )
          : null,
    );
  }
}

class _SidebarItem extends StatelessWidget {
  final IconData icon;
  final String label;
  final VoidCallback onTap;
  final bool isActive;

  const _SidebarItem({
    super.key,
    required this.icon,
    required this.label,
    required this.onTap,
    this.isActive = false,
  });

  @override
  Widget build(BuildContext context) {
    return Material(
      color: Colors.transparent,
      child: ListTile(
        leading: Icon(icon, color: isActive ? Colors.white : Colors.white70),
        title: Text(
          label,
          style: TextStyle(
            color: isActive ? Colors.white : Colors.white70,
            fontWeight: isActive ? FontWeight.bold : FontWeight.normal,
          ),
        ),
        onTap: onTap,
        tileColor: isActive ? Colors.white.withOpacity(0.1) : null,
        hoverColor: Colors.white12,
      ),
    );
  }
}
