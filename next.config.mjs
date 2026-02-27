/** @type {import('next').NextConfig} */
const nextConfig = {
    reactStrictMode: false,
    output: 'standalone',
    async rewrites() {
        const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
        return [
            // Proxy public blog endpoints (RSS, sitemap)
            {
                source: '/public/blog/rss',
                destination: `${API_URL}/public/blog/rss`,
            },
            {
                source: '/public/blog/sitemap.xml',
                destination: `${API_URL}/public/blog/sitemap.xml`,
            },
        ];
    },
};

export default nextConfig;
