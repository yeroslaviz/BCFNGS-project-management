
#First change the ldap_string to accept `ldaps://` instead of `ldap://`

ldap_string_fixed <- function(host, port) {
    paste0("ldaps://", host, ":", port)
  }

assignInNamespace(
  x = "ldap_string",
  value = ldap_string_fixed,
  ns = "ldapr"
)
# chceck the function 
ldapr:::ldap_string

# Temporary test function - remove after testing
test_ldap <- function() {
  test_user <- "yeroslaviz"  # Your username
  test_password <- "/q%8gFLS"  # Your actual password
  
  result <- validate_ldap_credentials(test_user, test_password)
  cat("LDAP test result class:", class(result), "\n")
  cat("LDAP test result type:", typeof(result), "\n")
  cat("LDAP test result contents:\n")
  print(result)
}

# Run the test
test_ldap()

################################

ldap.forumsys.com Port: 389

Bind DN: cn=read-only-admin,dc=example,dc=com Bind Password: password

All user passwords are password.

You may also bind to individual Users (uid) or the two Groups (ou) that include:
  
  ou=mathematicians,dc=example,dc=com

riemann
gauss
euler
euclid
ou=scientists,dc=example,dc=com

einstein
newton
galieleo
tesla

l <- ldap$new(
  host = "ldap.forumsys.com",
  base_dn = "cn=read-only-admin,dc=example,dc=com"
)
l
#> <LDAP connection>
#>   URI: ldap://zflexldap.com:389
#>   Authenticated: FALSE
Authenticating against the LDAP server is then easy with envoking the bind method.

l$bind(
  user = "riemann",
  pw = "password",
  "uid"
)

###############################


dn: uid=yeroslaviz,ou=Cox,ou=Research Group,ou=People,dc=biochem,dc=mpg,dc=de

ldapsearch -H ldaps://ldapserv1.biochem.mpg.de:636 \
-D "uid=yeroslaviz,ou=Cox,ou=Research Group,ou=People,dc=biochem,dc=mpg,dc=de" \
-W \
-b "dc=biochem,dc=mpg,dc=de" "(objectClass=*)"

