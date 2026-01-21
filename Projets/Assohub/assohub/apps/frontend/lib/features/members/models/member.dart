class Member {
  final String id;
  final String email;
  final String? firstName;
  final String? lastName;
  final String role;
  final String status;
  final DateTime? createdAt;

  Member({
    required this.id,
    required this.email,
    this.firstName,
    this.lastName,
    required this.role,
    required this.status,
    this.createdAt,
  });

  factory Member.fromJson(Map<String, dynamic> json) {
    return Member(
      id: json['id'],
      email: json['email'],
      firstName: json['firstName'],
      lastName: json['lastName'],
      role: json['role'],
      status: json['status'],
      createdAt: json['createdAt'] != null ? DateTime.parse(json['createdAt']) : null,
    );
  }

  String get fullName => '${firstName ?? ""} ${lastName ?? ""}'.trim();
}
