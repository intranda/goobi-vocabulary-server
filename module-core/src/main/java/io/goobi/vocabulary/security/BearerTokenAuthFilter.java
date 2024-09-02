package io.goobi.vocabulary.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
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
    @Value("${security.token}")
    private String secretToken;

    @Value("${security.anonymous.read-allowed}")
    private boolean anonymousReadAllowed;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        final String authHeader = request.getHeader("Authorization");

        try {
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
        } catch (Exception exception) {
//            handlerExceptionResolver.resolveException(request, response, null, exception);
//            exception.printStackTrace();
        }
    }

    private boolean isPublic(HttpServletRequest request) {
        // TODO: Vocabulary filtering
        return anonymousReadAllowed && "GET".equals(request.getMethod());
    }

    private boolean isTokenValid(String accessToken) {
        return secretToken.equals(accessToken);
    }
}