package io.goobi.vocabulary.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Optional;

@Component
public class BearerTokenAuthFilter extends OncePerRequestFilter {
    @Value("${security.token:#{null}}")
    private String secretToken;

    @Value("${security.anonymous.read-allowed:false}")
    private boolean anonymousReadAllowed;

    @Bean
    public FilterRegistrationBean<BearerTokenAuthFilter> bearerTokenAuthFilterFilterRegistrationBean(BearerTokenAuthFilter filter) {
        FilterRegistrationBean<BearerTokenAuthFilter> registration = new FilterRegistrationBean<>(filter);
        registration.setEnabled(false);
        return registration;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        final String authHeader = request.getHeader("Authorization");

        final String token = authHeader != null && authHeader.startsWith("Bearer ") ? authHeader.substring(7) : null;
        final Optional<String> jwt = Optional.ofNullable(token);

        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null) {
            User user = new User();

            if (isPublic(request) || (jwt.isPresent() && isTokenValid(jwt.get()))) {
                UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
                        user,
                        null,
                        user.getAuthorities()
                );

                authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(authToken);
            }
        }
        filterChain.doFilter(request, response);
    }

    private boolean isPublic(HttpServletRequest request) {
        // TODO: Vocabulary filtering
        return anonymousReadAllowed && "GET".equals(request.getMethod());
    }

    private boolean isTokenValid(String accessToken) {
        // If secret token is not set, deny
        if (secretToken == null) {
            return false;
        }
        return secretToken.equals(accessToken);
    }
}
