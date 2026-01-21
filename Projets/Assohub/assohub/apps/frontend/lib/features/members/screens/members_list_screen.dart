import 'package:flutter/material.dart';
import '../models/member.dart';
import '../services/members_service.dart';

class MembersListScreen extends StatefulWidget {
  const MembersListScreen({super.key});

  @override
  State<MembersListScreen> createState() => MembersListScreenState();
}

class MembersListScreenState extends State<MembersListScreen> {
  final MembersService _service = MembersService();
  List<Member> _members = [];
  bool _isLoading = true;
  String? _error;

  @override
  void initState() {
    super.initState();
    _loadMembers();
  }

  Future<void> _loadMembers() async {
    setState(() {
      _isLoading = true;
      _error = null;
    });
    try {
      final members = await _service.getMembers();
      setState(() {
        _members = members;
        _isLoading = false;
      });
    } catch (e) {
      setState(() {
        _error = "Erreur lors du chargement des membres";
        _isLoading = false;
      });
    }
  }

  void showAddMemberDialog(BuildContext context) {
    final emailController = TextEditingController();
    final firstNameController = TextEditingController();
    final lastNameController = TextEditingController();
    String selectedRole = 'MEMBER';

    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Ajouter un membre'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            TextField(
              controller: emailController,
              decoration: const InputDecoration(labelText: 'Email'),
            ),
            TextField(
              controller: firstNameController,
              decoration: const InputDecoration(labelText: 'Prénom'),
            ),
            TextField(
              controller: lastNameController,
              decoration: const InputDecoration(labelText: 'Nom'),
            ),
            DropdownButtonFormField<String>(
              value: selectedRole,
              items: const [
                DropdownMenuItem(value: 'MEMBER', child: Text('Membre')),
                DropdownMenuItem(value: 'ADMIN', child: Text('Admin')),
                DropdownMenuItem(value: 'TREASURER', child: Text('Trésorier')),
                DropdownMenuItem(value: 'SECRETARY', child: Text('Secrétaire')),
              ],
              onChanged: (val) => selectedRole = val!,
              decoration: const InputDecoration(labelText: 'Rôle'),
            ),
          ],
        ),
        actions: [
          TextButton(onPressed: () => Navigator.pop(context), child: const Text('Annuler')),
          ElevatedButton(
            onPressed: () async {
              try {
                await _service.createMember(
                  email: emailController.text,
                  firstName: firstNameController.text,
                  lastName: lastNameController.text,
                  role: selectedRole,
                );
                Navigator.pop(context);
                _loadMembers();
              } catch (e) {
                ScaffoldMessenger.of(context).showSnackBar(
                  const SnackBar(content: Text('Erreur lors de la création')),
                );
              }
            },
            child: const Text('Ajouter'),
          ),
        ],
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Expanded(
          child: _isLoading
              ? const Center(child: CircularProgressIndicator())
              : _error != null
                  ? Center(
                      child: Column(
                        mainAxisAlignment: MainAxisAlignment.center,
                        children: [
                          Text(_error!, style: const TextStyle(color: Colors.red)),
                          ElevatedButton(onPressed: _loadMembers, child: const Text('Réessayer')),
                        ],
                      ),
                    )
                  : SingleChildScrollView(
                      padding: const EdgeInsets.symmetric(horizontal: 32),
                      child: Card(
                        child: DataTable(
                          columns: const [
                            DataColumn(label: Text('Nom')),
                            DataColumn(label: Text('Email')),
                            DataColumn(label: Text('Rôle')),
                            DataColumn(label: Text('Statut')),
                            DataColumn(label: Text('Actions')),
                          ],
                          rows: _members
                              .map((m) => DataRow(cells: [
                                    DataCell(Text(m.fullName)),
                                    DataCell(Text(m.email)),
                                    DataCell(TagWidget(label: m.role, color: Colors.blue)),
                                    DataCell(TagWidget(
                                        label: m.status,
                                        color: m.status == 'ACTIVE' ? Colors.green : Colors.grey)),
                                    DataCell(IconButton(
                                      icon: const Icon(Icons.delete, color: Colors.red),
                                      onPressed: () async {
                                        await _service.deleteMember(m.id);
                                        _loadMembers();
                                      },
                                    )),
                                  ]))
                              .toList(),
                        ),
                      ),
                    ),
        ),
      ],
    );
  }
}

class TagWidget extends StatelessWidget {
  final String label;
  final Color color;
  const TagWidget({super.key, required this.label, required this.color});

  @override
  Widget build(BuildContext context) {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 4),
      decoration: BoxDecoration(
        color: color.withOpacity(0.1),
        borderRadius: BorderRadius.circular(12),
        border: Border.all(color: color),
      ),
      child: Text(
        label,
        style: TextStyle(color: color, fontSize: 12, fontWeight: FontWeight.bold),
      ),
    );
  }
}
